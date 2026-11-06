from intbase import InterpreterBase, ErrorType 
from brewparse import parse_program 

'''Tree should be in this shape:
Prog(root) - fct: f1, f2, f3 - fctNode: main, otherFct - stmNode: stm1, stm2'''
class Interpreter (InterpreterBase):
    def __init__(self, console_output=True, inp=None, trace_output=False):
        # This is InterpreterBase's constructor 
        super().__init__(console_output, inp)
        # Use dict to hold var name and var val 
        self.variable_name_to_value = {} 
        self.trace_output = trace_output

######## Program Node Area
    def run(self, program):
        # Parse program to AST, now var ast hold the prog as tree of nodes
        # This is the Program Node
        ast = parse_program(program) 
        # Check if prog node is valid
        if ast.elem_type != 'program':
            super().error(
                ErrorType.TYPE_ERROR,
                "Not found program node")
            return 
        # Get a Dict to hold node and vals 
        self.variable_name_to_value = {} 
        # Set Fct Def Node into a Dictionary
        main_func_node = self.get_main_func_node(ast)
        # Check if main() exists 
        if main_func_node is None:
              super().error(ErrorType.NAME_ERROR, "No main function defined")
              return
        self.run_func(main_func_node) # self.

######## Fct Node Area 
    def get_main_func_node(self, ast): #Find and return the main function node from the ast
        for func in ast.dict['functions']: # Looking from list of fct def node 
              if func.dict['name'] == 'main': # filter main() out 
                    return func
        return None 
          
    def run_func(self, func_node): # Run all stms node in a stm list in fct node 
        # This is Statement Node (in dict look for stms)
        for statement_node in func_node.dict['statements']: 
            self.run_statement(statement_node)
     
######## Stm Node Area 
    def run_statement(self, statement_node): # Run a single statement based on its type
        if self.is_definition(statement_node):
            self.do_definition(statement_node)
        elif self.is_assignment(statement_node):
            self.do_assignment(statement_node)
        elif self.is_func_call(statement_node):
            self.do_func_call(statement_node)
        else:
            super().error(
                ErrorType.TYPE_ERROR, 
                f"Unknown statement type: {statement_node.elem_type}"
            )

    def is_definition(self, statement_node):
        # This is Asgn Stm Node 
        return statement_node.elem_type == 'vardef'
    
    def is_assignment(self, statement_node):
        return statement_node.elem_type == '='
    
    def is_func_call(self,statement_node):
        #This is Fct Call Stm Node
        return statement_node.elem_type == 'fcall'
    
    def do_definition(self,statement_node):
        #"""Handle variable definition"""
        var_name = statement_node.dict['name']

        if self.var_name_exists(var_name):
            super().error(
              ErrorType.NAME_ERROR, 
              f"Variable {var_name} defined more than once"
            )
            #return

        # Initialize variable to None (undefined)
        self.variable_name_to_value[var_name] = None

    def do_assignment(self, statement_node):
	    # target_var_name = get_target_variable_name(statement_node)
        target_var_name = self.get_target_variable_name(statement_node)


        #   if !var_name_exists(target_var_name):
        #       generate_error("Undefined variable " + target_var_name)
        #	source_node = get_expression_node(statement_node)
        # 	resulting_value = evaluate_expression(source_node)
        #	this.variable_name_to_value[target_var_name] = resulting_value'''
        if not self.var_name_exists(target_var_name):
            super().error(
                ErrorType.NAME_ERROR, 
                f"Undefined variable {target_var_name}"
            )
            #return 
        source_node = self.get_expression_node(statement_node)
        resulting_value = self.evaluate_expression(source_node)
        self.variable_name_to_value[target_var_name] = resulting_value
        # [if resulting_value is not None:] 
        # -> we can conditionally update if evaluateion succeeded
        

    def do_func_call(self,statement_node):
        #handle fct call stm
        func_name = statement_node.dict['name']

        if func_name == 'print':
            self.do_print(statement_node)
        elif func_name == 'inputi': #inputi in stm context - elvaluate but ignore return val 
            self.evaluate_inputi(statement_node)
        else:
            super().error(
                ErrorType.NAME_ERROR, 
                f"Function {func_name} has not been defined"
            )


# not statement node
    def do_print(self, print_node):
        #"execute print function call"
        args = print_node.dict['args']
        output_parts = []
        '''for arg in args :
            value = self.evaluate_expression(arg)
            if value is None:
                output_parts.append(str(value))
                
        #concatenate all parts w/o spaces
        final_output = ''.join(output_parts)
        super().output(final_output)'''
        for arg in args:
            val = self.evaluate_expression(arg)
            # even if val == 0 or "", include it
            if val is None:
                super().error(ErrorType.TYPE_ERROR, "Cannot print None value")
            output_parts.append(str(val))
        super().output(''.join(output_parts))

    def get_target_variable_name(self, statement_node):
        return statement_node.dict['var']
    
    def get_expression_node(self, statement_node):
        return statement_node.dict['expression']
    
    def var_name_exists(self, var_name):
        return var_name in self.variable_name_to_value
    

######## Expression Node Area 
    def evaluate_expression(self, expression_node):
        if self.is_value_node(expression_node):
            return self.get_value(expression_node)
        elif self.is_variable_node(expression_node):
            return self.get_value_of_variable(expression_node)
        elif self.is_binary_operator(expression_node):
            return self.evaluate_binary_operator(expression_node)
        elif self.is_func_call_expression(expression_node):
            return self.evaluate_func_call_expression(expression_node)
        else:
            super().error(
                ErrorType.NAME_ERROR, 
                f"Unknown expression type: {expression_node.elem_type}"
            )
            return None
                 
    def is_value_node(self, expression_node):
        return expression_node.elem_type in ['int', 'string']

    def is_variable_node(self, expression_node):
        return expression_node.elem_type == 'qname' 

    def is_binary_operator(self, expression_node):
        return expression_node.elem_type in ['+', '-']

    def is_func_call_expression(self, expression_node):
        return expression_node.elem_type == 'fcall' 

    def get_value(self, value_node): #get val from int or str node
        #return value_node.dict['val']
        if value_node.elem_type == 'int':
            return int(value_node.dict['val'])
        elif value_node.elem_type == 'string':
            return value_node.dict['val']
        else:
            super().error(
                ErrorType.TYPE_ERROR,
                f"Unknown literal type: {value_node.elem_type}"
            )

    def get_value_of_variable(self, variable_node):
        var_name = variable_node.dict['name'] # get val of a var

        if not self.var_name_exists(var_name):
            super().error(
                ErrorType.NAME_ERROR, 
                f"Variable {var_name} is not defined"
            )
        if self.variable_name_to_value[var_name] is None:
            super().error(
                ErrorType.NAME_ERROR,
                f"Variable {var_name} has no value"
            )
        return self.variable_name_to_value[var_name] 

    def evaluate_binary_operator(self, op_node): #evaluate binary arithmetic operation
        op1_value = self.evaluate_expression(op_node.dict['op1'])
        op2_value = self.evaluate_expression(op_node.dict['op2'])

        if op1_value is None or op2_value is None: #check if both vals are valid
            return None 
        if type(op1_value) != int or type(op2_value) != int: # check if both are int 
            super().error(
                ErrorType.TYPE_ERROR, 
                "Incompatible types for arithmetic operation"
            )
            return None

        #now perform the operation
        if op_node.elem_type == '+':
            return op1_value + op2_value
        elif op_node.elem_type == '-':
            return op1_value - op2_value

    def evaluate_func_call_expression(self, func_call_node):
        func_name = func_call_node.dict['name'] #evaluate fct call in expr context

        if func_name == 'inputi':
            return self.evaluate_inputi(func_call_node)
        else:
            super().error(
                ErrorType.NAME_ERROR, 
                f"Function {func_name} has not been defined"
            )
            return None
        
    def evaluate_inputi(self, inputi_node):
        args = inputi_node.dict['args']

        #check if nums of arg
        if len(args) > 1:
            super().error(
                ErrorType.TYPE_ERROR, 
                "No inputi() function found that takes > 1 parameter"
            )
            #return None
        #output prompt if provided
        if len(args) == 1:
            prompt = self.evaluate_expression(args[0])
            if prompt is None:
                #return None
                super().error(ErrorType.TYPE_ERROR, "Prompt cannot be None")
            if not isinstance(prompt, str):
                super().error(
                    ErrorType.TYPE_ERROR, 
                    "Inputi prompt must be a string"
                )
                #return None
            super().output(prompt)

        #get input from user
        user_input = super().get_input()

        try:
            return int(user_input) #convert to int
        except ValueError: # This shouldn't happen per spec, but handle gracefully
            super().error(
                ErrorType.TYPE_ERROR, 
                "Input must be a valid integer"
            )
            #return None

def main():
    # all programs will be provided to your interpreter as a python string, 
    # just as shown here.
    program_source = """def main() {
        var bar;
        bar = 5;
        print("The answer is: ", (10 + bar) - 6, "!");

    }"""

 
    interpreter = Interpreter()
    interpreter.run(program_source)

    parsed_program = parse_program(program_source)

if __name__ == "__main__":
    main()