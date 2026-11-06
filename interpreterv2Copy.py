from intbase import InterpreterBase, ErrorType
from brewparse import parse_program

generate_image = False

class Environment:
    def __init__(self):
        self.env = {}

    # define new variable at function scope
    def fdef(self, varname):
        if self.exists(varname):
            return False
        self.env[varname] = None
        return True

    def exists(self, varname):
        return varname in self.env

    def get(self, varname):
        if varname in self.env:
            return self.env[varname]
        return None

    def set(self, varname, value):
        if not self.exists(varname):
            return False
        self.env[varname] = value
        return True

class Interpreter(InterpreterBase):
    def __init__(self, console_output=True, inp=None, trace_output=False):
        super().__init__(console_output, inp)
        self.funcs = {}  # {name: element}
        self.env = Environment() 
        self.scopes = []  
        self.bops = {'+', '-', '*', '/', 
                    '==', '!=', 
                    '>', '>=', '<', '<=', 
                    '||', '&&'}
        self.trace_output = trace_output

    def run(self, program):
        # Check for empty program
        if not program.strip(): 
            return
            
        ast = parse_program(program, generate_image)

        if ast.elem_type != 'program':
            super().error(ErrorType.TYPE_ERROR, "Cannot found program node")
            return 
        
        # Store all functions
        for func in ast.dict['functions']:
            self.funcs[(func.dict['name'], len(func.dict['args']))] = func
            
        main_func_node = self.get_main_func_node(ast)
        if main_func_node is None:
            super().error(ErrorType.NAME_ERROR, "No main function defined")
            return
            
        # Initialize global scope and run main
        self.push_scope()
        self.run_func(main_func_node)
        self.pop_scope()

    def get_main_func_node(self, ast):
        for k in self.funcs:
            if k[0] == 'main':
                return self.funcs[k]
        return None 

    def run_func(self, func_node):
        # Handle function parameters
        param_names = [arg.dict['name'] for arg in func_node.dict['args']]
        self.push_scope({name: None for name in param_names})
        
        # Execute function statements
        return_occurred = False
        result = None
        for statement_node in func_node.dict['statements']:
            stmt_result = self.run_statement(statement_node)
            if stmt_result and stmt_result[1]:  # return occurred
                result = stmt_result[0]
                return_occurred = True
                break
                
        self.pop_scope()
        return result if return_occurred else None

    def run_statement(self, statement_node):
        if self.is_definition(statement_node):
            self.do_definition(statement_node)
            return (None, False)
        elif self.is_assignment(statement_node):
            self.do_assignment(statement_node)
            return (None, False)
        elif self.is_func_call(statement_node):
            result = self.do_func_call(statement_node)
            return (None, False)
        elif self.is_if(statement_node):
            return self.do_if(statement_node)
        elif self.is_while(statement_node):
            return self.do_while(statement_node)
        elif self.is_return(statement_node):
            return self.do_return(statement_node)
        else:
            super().error(ErrorType.TYPE_ERROR, f"Unknown statement type: {statement_node.elem_type}")
            return (None, False)

    def is_definition(self, statement_node):
        return statement_node.elem_type == 'vardef'
    
    def is_assignment(self, statement_node):
        return statement_node.elem_type == '='
    
    def is_func_call(self, statement_node):
        return statement_node.elem_type == 'fcall'
    
    def is_if(self, statement_node):
        return statement_node.elem_type == 'if'
    
    def is_while(self, statement_node):
        return statement_node.elem_type == 'while'
    
    def is_return(self, statement_node):
        return statement_node.elem_type == 'return'

    def do_definition(self, statement_node):
        var_name = statement_node.dict['name']
        # Check for duplicate in current scope - this should catch shadowing errors
        if self.var_name_exists_in_current_scope(var_name):
            super().error(ErrorType.NAME_ERROR, f"Variable {var_name} already defined")
        self.set_var_in_current_scope(var_name, None)

    def do_assignment(self, statement_node):
        target_var_name = self.get_target_variable_name(statement_node)
        if not self.var_name_exists(target_var_name):
            super().error(ErrorType.NAME_ERROR, f"Undefined variable {target_var_name}")
        source_node = self.get_expression_node(statement_node)
        resulting_value = self.evaluate_expression(source_node)
        self.set_var(target_var_name, resulting_value)

    def do_func_call(self, statement_node):
        func_name = statement_node.dict['name']
        args = statement_node.dict['args']

        if func_name == 'print':
            self.do_print(statement_node)
            return None
        elif func_name == 'inputi' or func_name == 'inputs':
            return self.evaluate_input_func(statement_node)
        else:
            if (func_name, len(args)) not in self.funcs:
                super().error(ErrorType.NAME_ERROR, f"Function {func_name} not found")
                return None
                
            func_def = self.funcs[(func_name, len(args))]
            param_names = [arg.dict['name'] for arg in func_def.dict['args']]
            arg_values = [self.evaluate_expression(arg) for arg in args]
            
            func_scope = dict(zip(param_names, arg_values))
            self.push_scope(func_scope)
            
            result = None
            return_occurred = False
            for stmt in func_def.dict['statements']:
                stmt_result = self.run_statement(stmt)
                if stmt_result and stmt_result[1]:
                    result = stmt_result[0]
                    return_occurred = True
                    break
                    
            self.pop_scope()
            return result if return_occurred else None

    def do_if(self, statement_node):
        cond = self.evaluate_expression(statement_node.dict['condition'])
        if not isinstance(cond, bool):
            super().error(ErrorType.TYPE_ERROR, "If condition must be boolean")
            
        self.push_scope()
        
        result = None
        return_occurred = False
        
        if cond:
            for stmt in statement_node.dict['statements']:
                stmt_result = self.run_statement(stmt)
                if stmt_result and stmt_result[1]:
                    result = stmt_result[0]
                    return_occurred = True
                    break
        elif 'else_statements' in statement_node.dict and statement_node.dict['else_statements']:
            for stmt in statement_node.dict['else_statements']:
                stmt_result = self.run_statement(stmt)
                if stmt_result and stmt_result[1]:
                    result = stmt_result[0]
                    return_occurred = True
                    break
                    
        self.pop_scope()
        return (result, return_occurred)

    def do_while(self, statement_node):
        result = None
        return_occurred = False
        
        while True:
            cond = self.evaluate_expression(statement_node.dict['condition'])
            if not isinstance(cond, bool):
                super().error(ErrorType.TYPE_ERROR, "While condition must be boolean")
                
            if return_occurred or not cond:
                break
                
            self.push_scope()
            for stmt in statement_node.dict['statements']:
                stmt_result = self.run_statement(stmt)
                if stmt_result and stmt_result[1]:
                    result = stmt_result[0]
                    return_occurred = True
                    break
            self.pop_scope()
            
            if return_occurred:
                break
                
        return (result, return_occurred)

    def do_return(self, statement_node):
        expr = statement_node.dict.get('expression')
        value = self.evaluate_expression(expr) if expr else None
        return (value, True)

    def do_print(self, print_node):
        args = print_node.dict['args']
        output_parts = []
        for arg in args:
            val = self.evaluate_expression(arg)
            if val is None:
                output_parts.append('')
            elif isinstance(val, bool):
                output_parts.append('true' if val else 'false')
            else:
                output_parts.append(str(val))
        super().output(''.join(output_parts))

    def get_target_variable_name(self, statement_node):
        var_node = statement_node.dict['var']
        if hasattr(var_node, 'dict') and 'name' in var_node.dict:
            return var_node.dict['name']
        return str(var_node)
    
    def get_expression_node(self, statement_node):
        return statement_node.dict['expression']

    # Scope management - FIXED VERSION
    def push_scope(self, initial_vars=None):
        if initial_vars is None:
            initial_vars = {}
        self.scopes.append(initial_vars)

    def pop_scope(self):
        if self.scopes:
            return self.scopes.pop()
        return None

    def var_name_exists(self, var_name):
        # Check if variable exists in any accessible scope
        for scope in reversed(self.scopes):
            if var_name in scope:
                return True
        return False

    def var_name_exists_in_current_scope(self, var_name):
        if not self.scopes:
            return False
        return var_name in self.scopes[-1]

    def set_var(self, var_name, value):
        # Set variable in the first scope where it's found (inner to outer)
        for scope in reversed(self.scopes):
            if var_name in scope:
                scope[var_name] = value
                return True
        return False

    def set_var_in_current_scope(self, var_name, value):
        if not self.scopes:
            return False
        self.scopes[-1][var_name] = value
        return True

    def get_var(self, var_name):
        # Get variable from the first scope where it's found (inner to outer)
        for scope in reversed(self.scopes):
            if var_name in scope:
                return scope[var_name]
        return None  # Variable doesn't exist

    # Expression evaluation
    def evaluate_expression(self, expression_node):
        if self.is_value_node(expression_node):
            return self.get_value(expression_node)
        elif self.is_variable_node(expression_node):
            return self.get_value_of_variable(expression_node)
        elif self.is_binary_operator(expression_node):
            return self.evaluate_binary_operator(expression_node)
        elif self.is_func_call_expression(expression_node):
            return self.evaluate_func_call_expression(expression_node)
        elif self.is_unary_operator(expression_node):
            return self.evaluate_unary_operator(expression_node)
        elif self.is_nil_node(expression_node):
            return None
        else:
            super().error(ErrorType.NAME_ERROR, f"Unknown expression type: {expression_node.elem_type}")
            return None

    def is_value_node(self, expression_node):
        return expression_node.elem_type in ['int', 'string', 'bool']

    def is_variable_node(self, expression_node):
        return expression_node.elem_type == 'qname' 

    def is_binary_operator(self, expression_node):
        return expression_node.elem_type in self.bops

    def is_func_call_expression(self, expression_node):
        return expression_node.elem_type == 'fcall'

    def is_unary_operator(self, expression_node):
        return expression_node.elem_type in ['neg', '!']

    def is_nil_node(self, expression_node):
        return expression_node.elem_type == 'nil'

    def get_value(self, value_node):
        if value_node.elem_type == 'int':
            return int(value_node.dict['val'])
        elif value_node.elem_type == 'string':
            return value_node.dict['val']
        elif value_node.elem_type == 'bool':
            return value_node.dict['val']
        else:
            super().error(ErrorType.TYPE_ERROR, f"Unknown literal type: {value_node.elem_type}")

    def get_value_of_variable(self, variable_node):
        var_name = variable_node.dict['name']
        value = self.get_var(var_name)
        # CRITICAL FIX: Only error if variable doesn't exist, not if it has None value
        if value is None and not self.var_name_exists(var_name):
            super().error(ErrorType.NAME_ERROR, f"Variable {var_name} is not defined")
        return value

    def evaluate_binary_operator(self, op_node):
        op1_value = self.evaluate_expression(op_node.dict['op1'])
        op2_value = self.evaluate_expression(op_node.dict['op2'])
        op_type = op_node.elem_type

        # Handle nil values
        if op1_value is None or op2_value is None:
            if op_type == '==':
                return op1_value is None and op2_value is None
            elif op_type == '!=':
                return op1_value is not None or op2_value is not None
            else:
                super().error(ErrorType.TYPE_ERROR, "Cannot use nil in this operation")

        # Equality operators
        if op_type == '==':
            return type(op1_value) == type(op2_value) and op1_value == op2_value
        elif op_type == '!=':
            return type(op1_value) != type(op2_value) or op1_value != op2_value

        # Type-specific operations with STRICT type checking
        if isinstance(op1_value, str) and isinstance(op2_value, str):
            if op_type == '+':
                return op1_value + op2_value
            super().error(ErrorType.TYPE_ERROR, "Invalid string operation")

        if isinstance(op1_value, int) and isinstance(op2_value, int):
            if op_type == '+': return op1_value + op2_value
            if op_type == '-': return op1_value - op2_value
            if op_type == '*': return op1_value * op2_value
            if op_type == '/': return op1_value // op2_value
            if op_type == '<': return op1_value < op2_value
            if op_type == '<=': return op1_value <= op2_value
            if op_type == '>': return op1_value > op2_value
            if op_type == '>=': return op1_value >= op2_value
            super().error(ErrorType.TYPE_ERROR, "Invalid integer operation")

        if isinstance(op1_value, bool) and isinstance(op2_value, bool):
            if op_type == '&&': return op1_value and op2_value
            if op_type == '||': return op1_value or op2_value
            super().error(ErrorType.TYPE_ERROR, "Invalid boolean operation")

        # If we get here, it's a type mismatch - generate TYPE_ERROR
        super().error(ErrorType.TYPE_ERROR, f"Type mismatch for {op_type} operation")

    def evaluate_unary_operator(self, op_node):
        op_value = self.evaluate_expression(op_node.dict['op1'])
        op_type = op_node.elem_type

        if op_type == 'neg':
            if not isinstance(op_value, int):
                super().error(ErrorType.TYPE_ERROR, "Negation requires integer")
            return -op_value
        elif op_type == '!':
            if not isinstance(op_value, bool):
                super().error(ErrorType.TYPE_ERROR, "Logical not requires boolean")
            return not op_value

    def evaluate_func_call_expression(self, func_call_node):
        return self.do_func_call(func_call_node)

    def evaluate_input_func(self, input_node):
        func_name = input_node.dict['name']
        args = input_node.dict['args']

        if len(args) > 1:
            super().error(ErrorType.NAME_ERROR, "Too many arguments for input function")

        if args:
            prompt = self.evaluate_expression(args[0])
            super().output(str(prompt))

        user_input = super().get_input()
        return int(user_input) if func_name == 'inputi' else user_input

def main():
    interpreter = Interpreter()

    # The code below is meant to help you test your interpreter on your own Brewin programs.
    # To run this main function, create a file test.br in the same directory and put Brewin code in it.
    with open("./test.br", "r") as f:
        program = f.read()

    global generate_image
    generate_image = True
    interpreter.run(program)

if __name__ == "__main__":
    main()