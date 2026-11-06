from intbase import InterpreterBase, ErrorType
from brewparse import parse_program
from element import Element
from copy import copy, deepcopy
import enum
import functools


class Type(enum.Enum):
    INT = 1
    STRING = 2
    BOOL = 3
    OBJECT = 4
    VOID = 5
    ERROR = 6
    FUNCTION = 7  # New type for functions

    @staticmethod
    def get_type(var_name):
        if not var_name:
            return Type.ERROR
        last_letter = var_name[-1]
        if last_letter == "i":
            return Type.INT
        if last_letter == "s":
            return Type.STRING
        if last_letter == "b":
            return Type.BOOL
        if last_letter == "o":
            return Type.OBJECT
        if last_letter == "f":
            return Type.FUNCTION
        if last_letter == "v":
            return Type.VOID  # only for functions
        if last_letter.isupper():  # Interface type
            return Type.OBJECT
        return Type.ERROR


class Value:
    def __init__(self, t, v=None, interface=None):
        if v is None:
            self.t = t
            self.v = self.__default_value_for_type(t)
        else:
            self.t = t
            self.v = v
        self.interface = interface  # For OBJECT type, can have interface constraint

    def set(self, other):
        self.t = other.t
        self.v = other.v
        self.interface = other.interface

    def __default_value_for_type(self, t):
        if t == Type.INT:
            return 0
        if t == Type.STRING:
            return ""
        elif t == Type.BOOL:
            return False
        elif t == Type.OBJECT:
            return None  # representing Nil
        elif t == Type.FUNCTION:
            return None  # nil function
        elif t == Type.VOID:
            return None
        raise Exception("invalid default value for type")

    def __eq__(self, other):
        # For equality comparisons
        if self.t != other.t:
            return False
        if self.t == Type.OBJECT:
            # For objects, compare references
            return self.v is other.v
        if self.t == Type.FUNCTION:
            # For functions, compare references
            return self.v is other.v
        return self.v == other.v
    
    def copy(self):
        """Create a copy of this value."""
        if self.t == Type.OBJECT:
            # For objects, we need to copy the object dictionary
            if self.v is None:
                return Value(Type.OBJECT, None, self.interface)
            else:
                # Copy the object dictionary
                copied_dict = {}
                for k, v in self.v.items():
                    if v.t == Type.OBJECT:
                        copied_dict[k] = v.copy()
                    else:
                        copied_dict[k] = Value(v.t, v.v)
                return Value(Type.OBJECT, copied_dict, self.interface)
        else:
            # For primitives and functions, create a new Value with the same data
            return Value(self.t, self.v, self.interface)


class Closure:
    """Represents a closure for lambda functions"""
    def __init__(self, func_ast, captured_env):
        self.func_ast = func_ast
        self.captured_env = captured_env
        self.id = id(self)  # Unique ID for equality comparisons


class Function:
    def __init__(self, func_ast, closure_env=None):
        self.return_type = self.__get_return_type(func_ast)
        # the args in the ast is a list of qualified name nodes
        self.formal_args = {}
        for a in func_ast.get("args"):
            ref = a.dict.get("ref", False) if hasattr(a, 'dict') else False
            self.formal_args[a.get("name")] = ref
        self.statements = func_ast.get("statements")
        self.name = func_ast.get("name")
        self.is_lambda = func_ast.get("name").startswith("lambda")
        self.closure_env = closure_env  # For lambdas - captured environment
        self.id = id(self)  # Unique ID for equality comparisons

    def __get_return_type(self, func_ast):
        name = func_ast.get("name")
        if name == "main":
            return Type.VOID
        if name.startswith("lambda"):
            return Type.get_type(name)  # lambdai -> INT, lambdao -> OBJECT, etc.
        return Type.get_type(name)


class Interface:
    def __init__(self, name, fields):
        self.name = name
        self.fields = fields  # list of field definitions


class Environment:
    def __init__(self):
        self.env = []

    def enter_block(self):
        self.env[-1].append({})

    def exit_block(self):
        self.env[-1].pop()

    def enter_func(self):
        self.env.append([{}])

    def exit_func(self):
        self.env.pop()

    # define new variable at function scope
    def fdef(self, varname, value):
        # Check if variable already exists in function scope (including parameters)
        if self.exists_in_function_scope(varname):
            return False
        top_env = self.env[-1]
        top_env[0][varname] = value
        return True

    # define new variable in top block
    def bdef(self, varname, value):
        # Check if variable already exists in current block
        if self.exists_in_current_block(varname):
            return False
        top_env = self.env[-1]
        top_env[-1][varname] = value
        return True

    def exists(self, varname):
        for block in self.env[-1]:
            if varname in block:
                return True
        return False

    def exists_in_function_scope(self, varname):
        """Check if variable exists in function scope (level 0)"""
        if not self.env:
            return False
        return varname in self.env[-1][0]

    def exists_in_current_block(self, varname):
        """Check if variable exists in current block"""
        if not self.env:
            return False
        return varname in self.env[-1][-1]

    def get(self, varname):
        top_env = self.env[-1]
        for block in reversed(top_env):  # Search from innermost to outermost
            if varname in block:
                return block[varname]
        return None

    def set(self, varname, value):
        if not self.exists(varname):
            return False
        top_env = self.env[-1]
        # Search from innermost to outermost
        for block in reversed(top_env):
            if varname in block:
                block[varname] = value
                return True
        return False

    def get_captured_vars(self):
        """Get all variables that should be captured by a lambda.
        Primitives are captured by value, objects by reference."""
        captured = {}
        for level, blocks in enumerate(reversed(self.env)):
            for block in blocks:
                for var_name, value in block.items():
                    # For primitives (int, string, bool, function), capture by value (make a copy)
                    if value.t in [Type.INT, Type.STRING, Type.BOOL, Type.FUNCTION]:
                        captured[var_name] = value.copy()
                    else:
                        # For objects, capture by reference (use the same Value object)
                        # This allows field mutations to be visible
                        captured[var_name] = value
        return captured


class Interpreter(InterpreterBase):
    def __init__(self, console_output=True, inp=None, trace_output=False):
        super().__init__(console_output, inp)
        self.funcs = {}  # Maps (name, param_sig) -> Function object
        self.env = Environment()
        self.bops = {"+", "-", "*", "/", "==", "!=", ">", ">=", "<", "<=", "||", "&&"}
        self.interfaces = {}  # Maps interface name -> Interface object
        self.current_selfo = None  # For method calls
        self.func_values = {}  # Maps function name -> list of overloaded functions

    def run(self, program):
        # Handle empty programs gracefully
        if not program.strip():
            return
        
        try:
            ast = parse_program(program)
        except SyntaxError as e:
            # Handle syntax error
            super().error(ErrorType.TYPE_ERROR, "Syntax error")
            return
        
        self.__create_interface_table(ast)
        self.__create_function_table(ast)
        call_element = Element(InterpreterBase.FCALL_NODE, name="main", args=[])
        self.__run_fcall(call_element)
        
    def __create_interface_table(self, ast):
        """Parse and store interface definitions"""
        # Check if interfaces key exists in AST
        if "interfaces" not in ast.dict:
            return
        
        interfaces = ast.get("interfaces")
        if not interfaces:  # Empty list or None
            return
        
        for interface_ast in interfaces:
            name = interface_ast.get("name")
            if len(name) != 1 or not name.isupper():
                super().error(ErrorType.NAME_ERROR, "Interface name must be single uppercase letter")
            
            if name in self.interfaces:
                super().error(ErrorType.NAME_ERROR, f"Interface {name} already defined")
            
            fields = []
            field_names = set()
            
            # Get fields if they exist
            if "fields" in interface_ast.dict:
                for field in interface_ast.get("fields"):
                    field_name = field.get("name")
                    if field_name in field_names:
                        super().error(ErrorType.NAME_ERROR, f"Duplicate field {field_name} in interface {name}")
                    field_names.add(field_name)
                    
                    if field.elem_type == "field_var":
                        fields.append({
                            "type": "var",
                            "name": field_name,
                            "field_type": Type.get_type(field_name)
                        })
                    else:  # field_func
                        # For function fields, we need to check parameter types
                        param_types = []
                        # Get params if they exist
                        if "params" in field.dict:
                            params = field.get("params")
                            for param in params:
                                param_name = param.get("name")
                                param_type = param_name[-1]
                                if param_type.isupper():
                                    # Interface parameter
                                    if param_type not in self.interfaces:
                                        super().error(ErrorType.NAME_ERROR, f"Interface {param_type} not defined")
                                # Get ref attribute safely
                                ref = False
                                if hasattr(param, 'dict') and "ref" in param.dict:
                                    ref = param.dict["ref"]
                                param_types.append({
                                    "name": param_name,
                                    "type": param_type,
                                    "ref": ref
                                })
                        fields.append({
                            "type": "func",
                            "name": field_name,
                            "params": param_types
                        })
            
            self.interfaces[name] = Interface(name, fields)

    def __get_parameters_type_signature(self, formal_params):
        # a formal arg is an Element of type ARG_NODE
        param_type_sig = []
        for p in formal_params:
            param_name = p.get("name")
            last_char = param_name[-1]
            # For overloading, interface types are treated as 'o'
            if last_char.isupper():
                param_type_sig.append('o')
            else:
                param_type_sig.append(last_char)
        return "".join(param_type_sig)

    def __get_arguments_type_signature(self, actual_args):
        arg_sig = ""
        for arg in actual_args:
            if arg.t == Type.INT:
                arg_sig += "i"
            elif arg.t == Type.STRING:
                arg_sig += "s"
            elif arg.t == Type.BOOL:
                arg_sig += "b"
            elif arg.t == Type.OBJECT:
                arg_sig += "o"
            elif arg.t == Type.FUNCTION:
                arg_sig += "f"
            elif arg.t == Type.VOID:
                super().error(
                    ErrorType.TYPE_ERROR, "void type not allowed as parameter"
                )
            else:
                raise Exception("shouldn't reach this!")
        return arg_sig

    def __create_function_table(self, ast):
        self.funcs = {}
        self.func_values = {}
        
        # Check if functions key exists in AST
        if "functions" not in ast.dict:
            super().error(ErrorType.NAME_ERROR, "No functions defined")
        
        # First pass: collect all functions
        for func in ast.get("functions"):
            name = func.get("name")
            if name.startswith("lambda"):
                continue  # Lambdas are handled separately
                
            # Get args safely
            args = []
            if "args" in func.dict:
                args = func.get("args")
                
            param_type_sig = self.__get_parameters_type_signature(args)
            func_obj = Function(func)
            
            if func_obj.return_type == Type.ERROR:
                super().error(ErrorType.TYPE_ERROR, f"Invalid return type for function {name}")
            
            type_sig = (name, param_type_sig)
            
            # Check for duplicate function definitions
            if type_sig in self.funcs:
                super().error(ErrorType.NAME_ERROR, f"Function {name} with signature {param_type_sig} already defined")
            
            self.funcs[type_sig] = func_obj
            
            # Store in func_values for overload resolution
            if name not in self.func_values:
                self.func_values[name] = []
            self.func_values[name].append(func_obj)

    def __get_function(self, name, param_type_signature="", exact_match=True):
        if exact_match:
            # For exact match (e.g., from function variable)
            if (name, param_type_signature) in self.funcs:
                return self.funcs[(name, param_type_signature)]
        else:
            # For overload resolution
            if name in self.func_values:
                # Find matching overload
                for func_obj in self.func_values[name]:
                    if len(param_type_signature) == len(func_obj.formal_args):
                        # Check type compatibility
                        if self.__check_param_compatibility(param_type_signature, func_obj):
                            return func_obj
        return None

    def __check_param_compatibility(self, arg_sig, func_obj):
        """Check if argument signature matches function parameters"""
        func_params = list(func_obj.formal_args.keys())
        if len(arg_sig) != len(func_params):
            return False
        
        for i, param_name in enumerate(func_params):
            expected_type = param_name[-1]
            actual_type_char = arg_sig[i]
            
            if expected_type.isupper():
                # Interface parameter - treated as 'o' for overloading
                expected_type = 'o'
            
            if actual_type_char != expected_type:
                return False
                
        return True

    def __check_interface_compliance(self, obj_value, interface_name):
        """Check if an object satisfies an interface"""
        if obj_value.v is None:  # nil
            return True
        
        if interface_name not in self.interfaces:
            return False
        
        interface = self.interfaces[interface_name]
        
        for field_spec in interface.fields:
            field_name = field_spec["name"]
            
            # Check if field exists in object
            if field_name not in obj_value.v:
                return False
            
            field_value = obj_value.v[field_name]
            
            if field_spec["type"] == "var":
                # Check variable field type
                expected_type = field_spec["field_type"]
                if field_value.t != expected_type:
                    return False
            else:  # function field
                # Check function field
                if field_value.t != Type.FUNCTION:
                    return False
                
                # Check parameter compatibility
                func_obj = field_value.v
                expected_params = field_spec["params"]
                
                if len(func_obj.formal_args) != len(expected_params):
                    return False
                
                # Compare parameter by parameter
                func_params = list(func_obj.formal_args.items())
                for i, (param_name, is_ref) in enumerate(func_params):
                    expected_param = expected_params[i]
                    
                    # Check reference qualifier
                    if is_ref != expected_param["ref"]:
                        return False
                    
                    # Check type
                    param_type = param_name[-1]
                    expected_type = expected_param["type"]
                    
                    # For interface parameters, need to check if the expected interface
                    # is compatible with the actual parameter type
                    if expected_type.isupper():
                        # Expected interface parameter
                        if param_type.isupper():
                            # Actual is also interface - must be same interface
                            if param_type != expected_type:
                                return False
                        elif param_type != 'o':
                            # Actual is not object or interface
                            return False
                    else:
                        # Expected primitive/object/function type
                        if param_type != expected_type:
                            # Allow object parameters to accept interface values
                            if not (expected_type == 'o' and param_type.isupper()):
                                return False
        
        return True

    def __run_vardef(self, statement, block_def=False):
        name = statement.get("name")
        var_type = Type.get_type(name)
        
        if var_type == Type.ERROR:
            super().error(ErrorType.TYPE_ERROR, "invalid variable type")

        # Set interface constraint if variable is interface-typed
        interface = None
        if var_type == Type.OBJECT and name[-1].isupper():
            interface = name[-1]
            if interface not in self.interfaces:
                super().error(ErrorType.NAME_ERROR, f"Interface {interface} not defined")

        default_value = Value(var_type, interface=interface)
        
        if block_def:
            if not self.env.bdef(name, default_value):
                super().error(ErrorType.NAME_ERROR, "variable already defined")
        else:
            if not self.env.fdef(name, default_value):
                super().error(ErrorType.NAME_ERROR, "variable already defined")

    def __run_assign(self, statement):
        name = statement.get("var")
        dotted_name = name.split(".")
        rvalue = self.eval_expr(statement.get("expression"))

        if not self.env.exists(dotted_name[0]):
            super().error(ErrorType.NAME_ERROR, "variable not defined")

        # Get lvalue type based on last component
        last_component = dotted_name[-1]
        expected_type = Type.get_type(last_component)
        
        # For interface types, we need to check compliance
        if expected_type == Type.OBJECT and last_component[-1].isupper():
            interface_name = last_component[-1]
            if not self.__check_interface_compliance(rvalue, interface_name):
                super().error(ErrorType.TYPE_ERROR, f"Object does not satisfy interface {interface_name}")
        elif expected_type != rvalue.t:
            # For non-interface types, check exact type match
            super().error(ErrorType.TYPE_ERROR, "type mismatch in assignment")

        if len(dotted_name) == 1:
            value = self.env.get(name)
            if value.t != rvalue.t:
                super().error(ErrorType.TYPE_ERROR, "type mismatch in assignment")
            
            # For interface variables, check compliance
            if value.interface is not None:
                if not self.__check_interface_compliance(rvalue, value.interface):
                    super().error(ErrorType.TYPE_ERROR, f"Object does not satisfy interface {value.interface}")
            
            value.set(rvalue)
            return

        # Handle dotted assignment
        lvalue = self.env.get(dotted_name[0])
        if lvalue.t != Type.OBJECT:
            super().error(ErrorType.TYPE_ERROR, "cannot access member of non-object")
        if lvalue.v is None:
            super().error(ErrorType.FAULT_ERROR, "cannot dereference nil object")

        # Navigate through intermediate objects
        for i in range(1, len(dotted_name) - 1):
            component = dotted_name[i]
            if component not in lvalue.v:
                super().error(ErrorType.NAME_ERROR, f"object member {component} not found")
            
            next_lvalue = lvalue.v[component]
            if next_lvalue.t != Type.OBJECT:
                super().error(ErrorType.TYPE_ERROR, "intermediate member must be an object")
            if next_lvalue.v is None:
                super().error(ErrorType.FAULT_ERROR, "cannot dereference nil member object")
            
            lvalue = next_lvalue

        # Set the final field
        final_field = dotted_name[-1]
        if rvalue.t == Type.OBJECT:
            lvalue.v[final_field] = rvalue
        else:
            lvalue.v[final_field] = Value(rvalue.t, rvalue.v)

    def __handle_input(self, fcall_name, args):
        """Handle inputi and inputs function calls"""
        if len(args) > 1:
            super().error(ErrorType.NAME_ERROR, "too many arguments for input function")

        if args:
            self.__handle_print(args)

        res = super().get_input()

        return (
            Value(Type.INT, int(res))
            if fcall_name == "inputi"
            else Value(Type.STRING, res)
        )

    def __handle_print(self, args):
        """Handle print function calls"""
        out = ""

        for arg in args:
            c_out = self.eval_expr(arg)
            if c_out.t == Type.VOID:
                super().error(
                    ErrorType.TYPE_ERROR, "cannot pass void argument to function"
                )
            if c_out.t == Type.BOOL:
                out += str(c_out.v).lower()
            else:
                out += str(c_out.v)

        super().output(out)

        return Value(Type.VOID, None)

    def __run_fcall(self, func_call_ast, selfo_value=None, func_value=None):
        """Execute a function call. 
        
        Args:
            func_call_ast: The AST element for the function call
            selfo_value: Optional selfo value for method calls
            func_value: Optional pre-extracted function value (for method calls)
        """
        fcall_name, args = func_call_ast.get("name"), func_call_ast.get("args")
        
        # Handle built-in functions
        if fcall_name == "inputi" or fcall_name == "inputs":
            return self.__handle_input(fcall_name, args)

        if fcall_name == "print":
            return self.__handle_print(args)

        # If we already have a function value (from method call), use it
        if func_value is not None:
            actual_args = [self.eval_expr(a) for a in args]
            return self.__call_function(func_value, actual_args, selfo_value)

        # Try to evaluate fcall_name as a qualified name (could be function variable)
        try:
            # Check if this is a qualified name (e.g., o.funcf or just funcf)
            var_value = self.__get_var_value(Element("qname", name=fcall_name))
            
            if var_value is not None and var_value.t == Type.FUNCTION:
                # This is a function variable call
                if var_value.v is None:
                    super().error(ErrorType.FAULT_ERROR, "calling nil function")
                
                func_obj = var_value.v
                actual_args = [self.eval_expr(a) for a in args]
                
                # Check argument count
                if len(actual_args) != len(func_obj.formal_args):
                    super().error(ErrorType.TYPE_ERROR, "wrong number of arguments")
                
                # Call the function
                return self.__call_function(func_obj, actual_args, selfo_value)
        except:
            # Not a qualified name or not a function variable, try as named function
            pass

        # Handle as named function with overload resolution
        actual_args = [self.eval_expr(a) for a in args]
        args_type_sig = self.__get_arguments_type_signature(actual_args)
        
        # Try to find matching overload
        func_def = self.__get_function(fcall_name, args_type_sig, exact_match=False)
        if func_def is None:
            super().error(ErrorType.NAME_ERROR, f"function {fcall_name} not found")
        
        return self.__call_function(func_def, actual_args, selfo_value)

    def __call_function(self, func_def, actual_args, selfo_value=None):
        """Call a function with given arguments"""
        # Save current selfo and set new one if provided
        old_selfo = self.current_selfo
        if selfo_value is not None:
            self.current_selfo = selfo_value
        
        self.env.enter_func()
        
        # Handle lambda closure environment
        if func_def.is_lambda and func_def.closure_env is not None:
            # Set up captured variables
            for var_name, var_value in func_def.closure_env.items():
                # For captured primitives, we already have a copy
                # For captured objects, we use the same Value object (by reference)
                self.env.fdef(var_name, var_value)
        
        # Set up parameters
        formal_names = list(func_def.formal_args.keys())
        for formal, actual in zip(formal_names, actual_args):
            is_ref = func_def.formal_args[formal]
            
            # Check parameter type compatibility
            expected_type = Type.get_type(formal)
            if expected_type == Type.OBJECT and formal[-1].isupper():
                # Interface parameter
                interface_name = formal[-1]
                if not self.__check_interface_compliance(actual, interface_name):
                    super().error(ErrorType.TYPE_ERROR, f"Parameter does not satisfy interface {interface_name}")
            elif expected_type != actual.t:
                super().error(ErrorType.TYPE_ERROR, f"Parameter type mismatch for {formal}")
            
            if is_ref:
                # Pass by reference
                self.env.fdef(formal, actual)
            else:
                # Pass by value (copy)
                if actual.t == Type.OBJECT:
                    # For objects, we need to copy the object dictionary
                    if actual.v is None:
                        copied = Value(Type.OBJECT, None, actual.interface)
                    else:
                        # Deep copy the object
                        copied_dict = {}
                        for k, v in actual.v.items():
                            if v.t == Type.OBJECT:
                                copied_dict[k] = Value(v.t, deepcopy(v.v), v.interface)
                            else:
                                copied_dict[k] = Value(v.t, v.v)
                        copied = Value(Type.OBJECT, copied_dict, actual.interface)
                    self.env.fdef(formal, copied)
                else:
                    # For primitives and functions, shallow copy is fine
                    self.env.fdef(formal, Value(actual.t, actual.v, actual.interface))
        
        # Add selfo to environment if this is a method call
        if self.current_selfo is not None:
            self.env.fdef("selfo", self.current_selfo)
        
        # Execute function body
        res, _ = self.__run_statements(func_def, func_def.statements)
        self.env.exit_func()
        
        # Restore previous selfo
        self.current_selfo = old_selfo
        
        # Check return type
        if func_def.return_type != Type.VOID:
            if func_def.return_type == Type.OBJECT and func_def.name[-1].isupper():
                # Function returns interface type
                interface_name = func_def.name[-1]
                if not self.__check_interface_compliance(res, interface_name):
                    super().error(ErrorType.TYPE_ERROR, f"Return value does not satisfy interface {interface_name}")
            elif res.t != func_def.return_type:
                super().error(ErrorType.TYPE_ERROR, "return type mismatch")
        
        return res

    def __clone_for_passing(self, arg, ref_param):
        if ref_param:
            return arg  # pass by reference
        return Value(arg.t, arg.v, arg.interface)

    def __run_if(self, funcdef, statement):
        cond = self.eval_expr(statement.get("condition"))

        if cond.t != Type.BOOL:
            super().error(ErrorType.TYPE_ERROR, "condition must be boolean")

        self.env.enter_block()

        res, ret = Value(funcdef.return_type), False

        if cond.v:
            res, ret = self.__run_statements(funcdef, statement.get("statements"))
        elif statement.get("else_statements"):
            res, ret = self.__run_statements(funcdef, statement.get("else_statements"))

        self.env.exit_block()

        return res, ret

    def __run_while(self, funcdef, statement):
        res, ret = Value(funcdef.return_type), False

        while True:
            cond = self.eval_expr(statement.get("condition"))

            if cond.t != Type.BOOL:
                super().error(ErrorType.TYPE_ERROR, "condition must be boolean")

            if not cond.v:
                break

            self.env.enter_block()
            res, ret = self.__run_statements(funcdef, statement.get("statements"))
            self.env.exit_block()
            if ret:
                break

        return res, ret

    def __run_return(self, funcdef, statement):
        expr = statement.get("expression")
        if not expr:
            # Return default value based on return type
            if funcdef.return_type == Type.OBJECT and funcdef.name[-1].isupper():
                # Interface return type
                interface_name = funcdef.name[-1]
                return (Value(Type.OBJECT, None, interface=interface_name), True)
            return (Value(funcdef.return_type), True)
        
        result_val = self.eval_expr(expr)
        
        # Check return type
        if funcdef.return_type == Type.OBJECT and funcdef.name[-1].isupper():
            # Interface return type
            interface_name = funcdef.name[-1]
            if not self.__check_interface_compliance(result_val, interface_name):
                super().error(ErrorType.TYPE_ERROR, f"Return value does not satisfy interface {interface_name}")
        elif result_val.t != funcdef.return_type:
            super().error(ErrorType.TYPE_ERROR, "return type mismatch")
        
        return (result_val, True)

    def __run_statements(self, funcdef, statements):
        res, ret = Value(funcdef.return_type), False

        for statement in statements:
            kind = statement.elem_type

            if kind == self.VAR_DEF_NODE:
                self.__run_vardef(statement)
            elif kind == self.BVAR_DEF_NODE:
                self.__run_vardef(statement, True)
            elif kind == "=":
                self.__run_assign(statement)
            elif kind == self.FCALL_NODE:
                self.__run_fcall(statement)
            elif kind == self.IF_NODE:
                res, ret = self.__run_if(funcdef, statement)
                if ret:
                    break
            elif kind == self.WHILE_NODE:
                res, ret = self.__run_while(funcdef, statement)
                if ret:
                    break
            elif kind == self.RETURN_NODE:
                res, ret = self.__run_return(funcdef, statement)
                break

        return res, ret

    def __eval_binary_op(self, kind, vl, vr):
        """Evaluate binary operations"""
        tl, tr = vl.t, vr.t
        vl_val, vr_val = vl.v, vr.v

        if kind == "==":
            # Special handling for nil comparisons
            if tl == Type.OBJECT and vl_val is None and tr == Type.OBJECT and vr_val is None:
                return Value(Type.BOOL, True)
            if tl == Type.FUNCTION and vl_val is None and tr == Type.FUNCTION and vr_val is None:
                return Value(Type.BOOL, True)
            if (tl == Type.OBJECT and vl_val is None) and (tr == Type.FUNCTION and vr_val is None):
                return Value(Type.BOOL, True)
            if (tl == Type.FUNCTION and vl_val is None) and (tr == Type.OBJECT and vr_val is None):
                return Value(Type.BOOL, True)
            
            # For objects and functions, compare references
            if tl == Type.OBJECT and tr == Type.OBJECT:
                return Value(Type.BOOL, vl_val is vr_val)
            if tl == Type.FUNCTION and tr == Type.FUNCTION:
                # Compare function references
                return Value(Type.BOOL, vl_val is vr_val)
            
            # For primitives, compare values
            return Value(Type.BOOL, tl == tr and vl_val == vr_val)
        
        if kind == "!=":
            # Just negate the equality result
            eq_result = self.__eval_binary_op("==", vl, vr)
            return Value(Type.BOOL, not eq_result.v)

        if tl == Type.STRING and tr == Type.STRING:
            if kind == "+":
                return Value(Type.STRING, vl_val + vr_val)

        if tl == Type.INT and tr == Type.INT:
            if kind == "+":
                return Value(Type.INT, vl_val + vr_val)
            if kind == "-":
                return Value(Type.INT, vl_val - vr_val)
            if kind == "*":
                return Value(Type.INT, vl_val * vr_val)
            if kind == "/":
                return Value(Type.INT, vl_val // vr_val)
            if kind == "<":
                return Value(Type.BOOL, vl_val < vr_val)
            if kind == "<=":
                return Value(Type.BOOL, vl_val <= vr_val)
            if kind == ">":
                return Value(Type.BOOL, vl_val > vr_val)
            if kind == ">=":
                return Value(Type.BOOL, vl_val >= vr_val)

        if tl == Type.BOOL and tr == Type.BOOL:
            if kind == "&&":
                return Value(Type.BOOL, vl_val and vr_val)
            if kind == "||":
                return Value(Type.BOOL, vl_val or vr_val)

        super().error(ErrorType.TYPE_ERROR, "invalid binary operation")

    def __eval_convert(self, expr):
        """Evaluate type conversion operations"""
        val = self.eval_expr(expr.get("expr"))
        to_type = expr.get("to_type")

        if to_type == "int":
            if val.t == Type.INT:
                return val
            elif val.t == Type.STRING:
                try:
                    return Value(Type.INT, int(val.v))
                except ValueError:
                    super().error(ErrorType.TYPE_ERROR, "cannot convert string to int")
            elif val.t == Type.BOOL:
                return Value(Type.INT, 1 if val.v else 0)
            else:
                super().error(ErrorType.TYPE_ERROR, "cannot convert object/function to int")

        elif to_type == "str":
            if val.t == Type.STRING:
                return val
            elif val.t == Type.INT:
                return Value(Type.STRING, str(val.v))
            elif val.t == Type.BOOL:
                return Value(Type.STRING, str(val.v).lower())
            else:
                super().error(ErrorType.TYPE_ERROR, "cannot convert object/function to string")

        elif to_type == "bool":
            if val.t == Type.BOOL:
                return val
            elif val.t == Type.INT:
                return Value(Type.BOOL, val.v != 0)
            elif val.t == Type.STRING:
                return Value(Type.BOOL, val.v != "")
            else:
                super().error(ErrorType.TYPE_ERROR, "cannot convert object/function to bool")
        else:
            super().error(ErrorType.TYPE_ERROR, "invalid conversion type")

    def __get_var_value(self, expr):
        dotted_name = expr.get("name").split(".")

        if not self.env.exists(dotted_name[0]):
            # Check if it's a function name
            if dotted_name[0] in self.func_values and len(dotted_name) == 1:
                # Return a function value
                # Note: For overloaded functions, we can't return a specific one
                # The spec says we won't test this ambiguous case
                func_list = self.func_values[dotted_name[0]]
                if len(func_list) == 1:
                    return Value(Type.FUNCTION, func_list[0])
                else:
                    # Ambiguous - return first one (per spec, we won't test this)
                    return Value(Type.FUNCTION, func_list[0])
            super().error(ErrorType.NAME_ERROR, "variable not defined")
        
        value = self.env.get(dotted_name[0])
        
        # Handle method calls - check if we're accessing a function field
        if len(dotted_name) > 1:
            # First component must be an object
            if value.t != Type.OBJECT:
                super().error(ErrorType.TYPE_ERROR, "cannot dereference a non-object")
            
            # Navigate through object chain
            for i in range(1, len(dotted_name)):
                component = dotted_name[i]
                if value.v is None:
                    super().error(ErrorType.FAULT_ERROR, "nil reference access")
                
                if component not in value.v:
                    super().error(ErrorType.NAME_ERROR, f"object member {component} not found")
                
                value = value.v[component]
                
                # Check intermediate components (except last)
                if i < len(dotted_name) - 1 and value.t != Type.OBJECT:
                    super().error(ErrorType.TYPE_ERROR, "intermediate member must be an object")
        
        return value

    def eval_expr(self, expr):
        kind = expr.elem_type

        if kind == self.INT_NODE:
            return Value(Type.INT, expr.get("val"))

        if kind == self.STRING_NODE:
            return Value(Type.STRING, expr.get("val"))

        if kind == self.BOOL_NODE:
            return Value(Type.BOOL, expr.get("val"))

        if kind == self.NIL_NODE:
            return Value(Type.OBJECT, None)

        if kind == self.EMPTY_OBJ_NODE:
            return Value(Type.OBJECT, {})

        if kind == self.QUALIFIED_NAME_NODE:
            return self.__get_var_value(expr)

        if kind == self.FCALL_NODE:
            # Check if this is a method call (dotted name)
            fcall_name = expr.get("name")
            if '.' in fcall_name:
                # This is a method call
                parts = fcall_name.split('.')
                if len(parts) < 2:
                    super().error(ErrorType.TYPE_ERROR, "invalid method call")
                
                # Get the object
                obj_expr = Element("qname", name='.'.join(parts[:-1]))
                obj_value = self.__get_var_value(obj_expr)
                
                if obj_value.t != Type.OBJECT:
                    super().error(ErrorType.TYPE_ERROR, "method call on non-object")
                if obj_value.v is None:
                    super().error(ErrorType.FAULT_ERROR, "method call on nil")
                
                # Check if the field exists and is a function
                method_name = parts[-1]
                if method_name not in obj_value.v:
                    super().error(ErrorType.NAME_ERROR, f"method {method_name} not found")
                
                method_value = obj_value.v[method_name]
                if method_value.t != Type.FUNCTION:
                    super().error(ErrorType.TYPE_ERROR, "calling non-function field")
                
                # We have the function value, so we can call it directly
                actual_args = [self.eval_expr(a) for a in expr.get("args")]
                return self.__call_function(method_value.v, actual_args, obj_value)
            else:
                # Regular function call
                return self.__run_fcall(expr)

        if kind in self.bops:
            l, r = self.eval_expr(expr.get("op1")), self.eval_expr(expr.get("op2"))
            return self.__eval_binary_op(kind, l, r)

        if kind == self.NEG_NODE:
            o = self.eval_expr(expr.get("op1"))
            if o.t == Type.INT:
                return Value(Type.INT, -o.v)
            super().error(ErrorType.TYPE_ERROR, "cannot negate non-integer")

        if kind == self.NOT_NODE:
            o = self.eval_expr(expr.get("op1"))
            if o.t == Type.BOOL:
                return Value(Type.BOOL, not o.v)
            super().error(ErrorType.TYPE_ERROR, "cannot apply NOT to non-boolean")

        if kind == self.CONVERT_NODE:
            return self.__eval_convert(expr)

        if kind == "func":
            # Lambda expression
            lambda_ast = expr
            # Capture current environment with proper copy semantics
            captured_vars = self.env.get_captured_vars()
            
            # Create closure with captured environment
            func_obj = Function(lambda_ast, closure_env=captured_vars)
            
            return Value(Type.FUNCTION, func_obj)

        raise Exception(f"Unknown expression type: {kind}")


def main():
    import sys

    interpreter = Interpreter()

    # Use command line argument if provided, otherwise default to test.br
    filename = sys.argv[1] if len(sys.argv) > 1 else "./test.br"

    with open(filename, "r") as f:
        program = f.read()

    interpreter.run(program)


if __name__ == "__main__":
    main() 