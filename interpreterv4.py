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
    FUNCTION = 7  

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
            return Type.VOID  
        if last_letter.isupper():  
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
        self.interface = interface  

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
            return None  
        elif t == Type.FUNCTION:
            return None  
        elif t == Type.VOID:
            return None
        raise Exception("invalid default value for type")

    def __eq__(self, other):
        if self.t != other.t:
            return False
        if self.t == Type.OBJECT:
            return self.v is other.v
        if self.t == Type.FUNCTION:
            return self.v is other.v
        return self.v == other.v
    
    def copy(self):
        if self.t == Type.OBJECT:
            if self.v is None:
                return Value(Type.OBJECT, None, self.interface)
            else:
                copied_dict = {}
                for k, v in self.v.items():
                    if v.t == Type.OBJECT:
                        copied_dict[k] = v.copy()
                    else:
                        copied_dict[k] = Value(v.t, v.v)
                return Value(Type.OBJECT, copied_dict, self.interface)
        else:
            return Value(self.t, self.v, self.interface)


class Closure:
    def __init__(self, func_ast, captured_env):
        self.func_ast = func_ast
        self.captured_env = captured_env
        self.id = id(self)  


class Function:
    def __init__(self, func_ast, closure_env=None):
        self.return_type = self.__get_return_type(func_ast)
        self.formal_args = {}
        for a in func_ast.get("args"):
            ref = a.dict.get("ref", False) if hasattr(a, 'dict') else False
            self.formal_args[a.get("name")] = ref
        self.statements = func_ast.get("statements")
        self.name = func_ast.get("name")
        self.is_lambda = func_ast.get("name").startswith("lambda")
        self.closure_env = closure_env  
        self.id = id(self)  

    def __get_return_type(self, func_ast):
        name = func_ast.get("name")
        if name == "main":
            return Type.VOID
        if name.startswith("lambda"):
            return Type.get_type(name)  
        return Type.get_type(name)


class Interface:
    def __init__(self, name, fields):
        self.name = name
        self.fields = fields 


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

    def fdef(self, varname, value):
        if self.exists_in_function_scope(varname):
            return False
        top_env = self.env[-1]
        top_env[0][varname] = value
        return True

    def bdef(self, varname, value):
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
        if not self.env:
            return False
        return varname in self.env[-1][0]

    def exists_in_current_block(self, varname):
        if not self.env:
            return False
        return varname in self.env[-1][-1]

    def get(self, varname):
        top_env = self.env[-1]
        for block in reversed(top_env):  
            if varname in block:
                return block[varname]
        return None

    def set(self, varname, value):
        if not self.exists(varname):
            return False
        top_env = self.env[-1]
        for block in reversed(top_env):
            if varname in block:
                block[varname] = value
                return True
        return False

    def get_captured_vars(self):
        captured = {}
        for level, blocks in enumerate(reversed(self.env)):
            for block in blocks:
                for var_name, value in block.items():
                    if value.t in [Type.INT, Type.STRING, Type.BOOL, Type.FUNCTION]:
                        captured[var_name] = value.copy()
                    else:
                        captured[var_name] = value
        return captured


class Interpreter(InterpreterBase):
    def __init__(self, console_output=True, inp=None, trace_output=False):
        super().__init__(console_output, inp)
        self.funcs = {}  
        self.env = Environment()
        self.bops = {"+", "-", "*", "/", "==", "!=", ">", ">=", "<", "<=", "||", "&&"}
        self.interfaces = {}  
        self.current_selfo = None 
        self.func_values = {}  

    def run(self, program):
        # Handle empty program 
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
        if "interfaces" not in ast.dict:
            return
        
        interfaces = ast.get("interfaces")
        if not interfaces: 
            return
        
        for interface_ast in interfaces:
            name = interface_ast.get("name")
            if len(name) != 1 or not name.isupper():
                super().error(ErrorType.NAME_ERROR, "Interface name must be single uppercase letter")
            
            if name in self.interfaces:
                super().error(ErrorType.NAME_ERROR, f"Interface {name} already defined")
            
            fields = []
            field_names = set()
            
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
                    else:  
                        param_types = []
                        if "params" in field.dict:
                            params = field.get("params")
                            for param in params:
                                param_name = param.get("name")
                                param_type = param_name[-1]
                                if param_type.isupper():
                                    if param_type not in self.interfaces:
                                        super().error(ErrorType.NAME_ERROR, f"Interface {param_type} not defined")
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
        param_type_sig = []
        for p in formal_params:
            param_name = p.get("name")
            last_char = param_name[-1]
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
        
        if "functions" not in ast.dict:
            super().error(ErrorType.NAME_ERROR, "No functions defined")
        
        for func in ast.get("functions"):
            name = func.get("name")
            if name.startswith("lambda"):
                continue  
                
            args = []
            if "args" in func.dict:
                args = func.get("args")
                
            param_type_sig = self.__get_parameters_type_signature(args)
            func_obj = Function(func)
            
            if func_obj.return_type == Type.ERROR:
                super().error(ErrorType.TYPE_ERROR, f"Invalid return type for function {name}")
            
            type_sig = (name, param_type_sig)
            
            if type_sig in self.funcs:
                super().error(ErrorType.NAME_ERROR, f"Function {name} with signature {param_type_sig} already defined")
            
            self.funcs[type_sig] = func_obj
            
            if name not in self.func_values:
                self.func_values[name] = []
            self.func_values[name].append(func_obj)

    def __get_function(self, name, param_type_signature="", exact_match=True):
        if exact_match:
            if (name, param_type_signature) in self.funcs:
                return self.funcs[(name, param_type_signature)]
        else:
            if name in self.func_values:
                for func_obj in self.func_values[name]:
                    if len(param_type_signature) == len(func_obj.formal_args):
                        if self.__check_param_compatibility(param_type_signature, func_obj):
                            return func_obj
        return None

    def __check_param_compatibility(self, arg_sig, func_obj):
        func_params = list(func_obj.formal_args.keys())
        if len(arg_sig) != len(func_params):
            return False
        
        for i, param_name in enumerate(func_params):
            expected_type = param_name[-1]
            actual_type_char = arg_sig[i]
            
            if expected_type.isupper():
                expected_type = 'o'
            
            if actual_type_char != expected_type:
                return False
                
        return True

    def __check_interface_compliance(self, obj_value, interface_name):
        if obj_value.v is None:  # nil
            return True
        
        if interface_name not in self.interfaces:
            return False
        
        interface = self.interfaces[interface_name]
        
        for field_spec in interface.fields:
            field_name = field_spec["name"]
            
            if field_name not in obj_value.v:
                return False
            
            field_value = obj_value.v[field_name]
            
            if field_spec["type"] == "var":
                expected_type = field_spec["field_type"]
                if field_value.t != expected_type:
                    return False
            else: 
                if field_value.t != Type.FUNCTION:
                    return False
                
                func_obj = field_value.v
                expected_params = field_spec["params"]
                
                if len(func_obj.formal_args) != len(expected_params):
                    return False
                
                func_params = list(func_obj.formal_args.items())
                for i, (param_name, is_ref) in enumerate(func_params):
                    expected_param = expected_params[i]
                    
                    if is_ref != expected_param["ref"]:
                        return False
                    
                    param_type = param_name[-1]
                    expected_type = expected_param["type"]
                    
                    if expected_type.isupper():
                        if param_type.isupper():
                            if param_type != expected_type:
                                return False
                        elif param_type != 'o':
                            return False
                    else:
                        if param_type != expected_type:
                            if not (expected_type == 'o' and param_type.isupper()):
                                return False
        
        return True

    def __run_vardef(self, statement, block_def=False):
        name = statement.get("name")
        var_type = Type.get_type(name)
        
        if var_type == Type.ERROR:
            super().error(ErrorType.TYPE_ERROR, "invalid variable type")

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

        last_component = dotted_name[-1]
        expected_type = Type.get_type(last_component)
        
        if expected_type == Type.OBJECT and last_component[-1].isupper():
            interface_name = last_component[-1]
            if not self.__check_interface_compliance(rvalue, interface_name):
                super().error(ErrorType.TYPE_ERROR, f"Object does not satisfy interface {interface_name}")
        elif expected_type != rvalue.t:
            super().error(ErrorType.TYPE_ERROR, "type mismatch in assignment")

        if len(dotted_name) == 1:
            value = self.env.get(name)
            if value.t != rvalue.t:
                super().error(ErrorType.TYPE_ERROR, "type mismatch in assignment")
            
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
        
        if fcall_name == "inputi" or fcall_name == "inputs":
            return self.__handle_input(fcall_name, args)

        if fcall_name == "print":
            return self.__handle_print(args)

        if func_value is not None:
            actual_args = [self.eval_expr(a) for a in args]
            return self.__call_function(func_value, actual_args, selfo_value)

        try:
            var_value = self.__get_var_value(Element("qname", name=fcall_name))
            
            if var_value is not None and var_value.t == Type.FUNCTION:
                if var_value.v is None:
                    super().error(ErrorType.FAULT_ERROR, "calling nil function")
                
                func_obj = var_value.v
                actual_args = [self.eval_expr(a) for a in args]
                
                if len(actual_args) != len(func_obj.formal_args):
                    super().error(ErrorType.TYPE_ERROR, "arguments count mismatch in assignment")
                
                return self.__call_function(func_obj, actual_args, selfo_value)
        except:
            pass

        actual_args = [self.eval_expr(a) for a in args]
        args_type_sig = self.__get_arguments_type_signature(actual_args)
        
        func_def = self.__get_function(fcall_name, args_type_sig, exact_match=False)
        if func_def is None:
            super().error(ErrorType.NAME_ERROR, f"function {fcall_name} not found")
        
        return self.__call_function(func_def, actual_args, selfo_value)

    def __call_function(self, func_def, actual_args, selfo_value=None):
        old_selfo = self.current_selfo
        if selfo_value is not None:
            self.current_selfo = selfo_value
        
        self.env.enter_func()
        
        # Handle lambda closure environment
        if func_def.is_lambda and func_def.closure_env is not None:
            for var_name, var_value in func_def.closure_env.items():
                self.env.fdef(var_name, var_value)
        
        formal_names = list(func_def.formal_args.keys())
        for formal, actual in zip(formal_names, actual_args):
            is_ref = func_def.formal_args[formal]
            
            expected_type = Type.get_type(formal)
            if expected_type == Type.OBJECT and formal[-1].isupper():
                interface_name = formal[-1]
                if not self.__check_interface_compliance(actual, interface_name):
                    super().error(ErrorType.TYPE_ERROR, f"Parameter does not satisfy interface {interface_name}")
            elif expected_type != actual.t:
                super().error(ErrorType.TYPE_ERROR, f"Parameter type mismatch for {formal}")
            
            if is_ref:
                # Pass by reference
                self.env.fdef(formal, actual)
            else:
                # Pass by value
                if actual.t == Type.OBJECT:
                    if actual.v is None:
                        copied = Value(Type.OBJECT, None, actual.interface)
                    else:
                        copied_dict = {}
                        for k, v in actual.v.items():
                            if v.t == Type.OBJECT:
                                copied_dict[k] = Value(v.t, deepcopy(v.v), v.interface)
                            else:
                                copied_dict[k] = Value(v.t, v.v)
                        copied = Value(Type.OBJECT, copied_dict, actual.interface)
                    self.env.fdef(formal, copied)
                else:
                    self.env.fdef(formal, Value(actual.t, actual.v, actual.interface))
        
        if self.current_selfo is not None:
            self.env.fdef("selfo", self.current_selfo)
        
        res, _ = self.__run_statements(func_def, func_def.statements)
        self.env.exit_func()
        
        self.current_selfo = old_selfo
        
        if func_def.return_type != Type.VOID:
            if func_def.return_type == Type.OBJECT and func_def.name[-1].isupper():
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
            if funcdef.return_type == Type.OBJECT and funcdef.name[-1].isupper():
                interface_name = funcdef.name[-1]
                return (Value(Type.OBJECT, None, interface=interface_name), True)
            return (Value(funcdef.return_type), True)
        
        result_val = self.eval_expr(expr)
        
        if funcdef.return_type == Type.OBJECT and funcdef.name[-1].isupper():
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
            
            if tl == Type.OBJECT and tr == Type.OBJECT:
                return Value(Type.BOOL, vl_val is vr_val)
            if tl == Type.FUNCTION and tr == Type.FUNCTION:
                return Value(Type.BOOL, vl_val is vr_val)
            
            return Value(Type.BOOL, tl == tr and vl_val == vr_val)
        
        if kind == "!=":
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
            if dotted_name[0] in self.func_values and len(dotted_name) == 1:
                func_list = self.func_values[dotted_name[0]]
                if len(func_list) == 1:
                    return Value(Type.FUNCTION, func_list[0])
                else:
                    return Value(Type.FUNCTION, func_list[0])
            super().error(ErrorType.NAME_ERROR, "variable not defined")
        
        value = self.env.get(dotted_name[0])
        
        if len(dotted_name) > 1:
            if value.t != Type.OBJECT:
                super().error(ErrorType.TYPE_ERROR, "cannot dereference a non-object")
            
            for i in range(1, len(dotted_name)):
                component = dotted_name[i]
                if value.v is None:
                    super().error(ErrorType.FAULT_ERROR, "nil reference access")
                
                if component not in value.v:
                    super().error(ErrorType.NAME_ERROR, f"object member {component} not found")
                
                value = value.v[component]
                
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
            fcall_name = expr.get("name")
            if '.' in fcall_name:
                parts = fcall_name.split('.')
                if len(parts) < 2:
                    super().error(ErrorType.TYPE_ERROR, "invalid method call")
                
                obj_expr = Element("qname", name='.'.join(parts[:-1]))
                obj_value = self.__get_var_value(obj_expr)
                
                if obj_value.t != Type.OBJECT:
                    super().error(ErrorType.TYPE_ERROR, "method call on non-object")
                if obj_value.v is None:
                    super().error(ErrorType.FAULT_ERROR, "method call on nil")
                
                method_name = parts[-1]
                if method_name not in obj_value.v:
                    super().error(ErrorType.NAME_ERROR, f"method {method_name} not found")
                
                method_value = obj_value.v[method_name]
                if method_value.t != Type.FUNCTION:
                    super().error(ErrorType.TYPE_ERROR, "calling non-function field")
                
                actual_args = [self.eval_expr(a) for a in expr.get("args")]
                return self.__call_function(method_value.v, actual_args, obj_value)
            else:
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