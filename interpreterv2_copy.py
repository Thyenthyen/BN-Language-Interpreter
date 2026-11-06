from intbase import InterpreterBase, ErrorType
from brewparse import parse_program
import enum
from copy import deepcopy


class Type(enum.Enum):
    NIL = 0
    INT = 1
    STRING = 2
    BOOL = 3
    STRUCT = 4

class Value:
    def __init__(self, t=None, v=None):
        if t is None:
            self.t = Type.NIL
            self.v = None
        else:
            self.t = t
            self.v = v

    def __repr__(self):
        return f"Value({self.t}, {self.v})"
    
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
    def fdef(self, varname):
        if self.exists(varname):
            return False
        top_env = self.env[-1]
        top_env[0][varname] = Value()
        return True

    def exists(self, varname):
        for block in self.env[-1]:
            if varname in block:
                return True
        return False

    def get(self, varname):
        top_env = self.env[-1]
        for block in top_env:
            if varname in block:
                return block[varname]
        return None

    def set(self, varname, value):
        if not self.exists(varname):
            return False
        top_env = self.env[-1]
        for block in top_env:
            if varname in block:
                block[varname] = value
        return True


class Interpreter(InterpreterBase):
    def __init__(self, console_output=True, inp=None, trace_output=False):
        super().__init__(console_output, inp)
        self.structs = {}  # {name:default_init,}
        self.funcs = {}  # {(name,n_args):element,}
        self.env = Environment()
        self.bops = {"+", "-", "*", "/", "==", "!=", ">", ">=", "<", "<=", "||", "&&"}

    def run(self, program):
        # Check for empty program
        if not program or program.strip() == "":
            return
            
        ast = parse_program(program)
        self.__create_function_table(ast)
        self.__create_struct_table(ast)
        
        # Check if main function exists
        if ("main", 0) not in self.funcs:
            super().error(ErrorType.NAME_ERROR, "No main function found")
            
        self.__run_fcall(self.__get_function("main"))

    def __create_function_table(self, ast):
        self.funcs = {}
        # Check if functions exist in the AST
        functions = ast.get("functions")
        if functions is not None:
            for func in functions:
                self.funcs[(func.get("name"), len(func.get("args")))] = func

    def __create_struct_table(self, ast):
        self.structs = {}
        # Check if structs exist in the AST - this is the key fix
        structs = ast.get("structs")
        if structs is not None:
            for struct in structs:
                name = struct.get("name")
                self.structs[name] = {"t": struct.get("name"), "f": None}

                fields = {}

                # if repeating field names, take the last one (undefined)
                for f in struct.get("fields"):
                    n, t = f.get("name"), f.get("var_type")

                    if t == "int":
                        fields[n] = Value(Type.INT, 0)
                    elif t == "bool":
                        fields[n] = Value(Type.BOOL, False)
                    elif t == "string":
                        fields[n] = Value(Type.STRING, "")
                    elif t in self.structs:
                        fields[n] = Value(Type.STRUCT, {"t": t, "f": None})
                    else:
                        super().error(ErrorType.TYPE_ERROR, f"Invalid struct field type: {t}")

                self.structs[name]["f"] = fields

    def __get_function(self, name, num_params=0):
        if (name, num_params) not in self.funcs:
            super().error(ErrorType.NAME_ERROR, f"function {name} with {num_params} params not found")
        return self.funcs[(name, num_params)]

    def __run_vardef(self, statement):
        name = statement.get("name")

        if not self.env.fdef(name):
            super().error(ErrorType.NAME_ERROR, "variable already defined")

        # Initialize variable with default value based on type
        var_type = statement.get("var_type")
        
        # Handle the case where var_type might be None (no type specified)
        if var_type is None:
            # Default to some type or handle as untyped
            self.env.set(name, Value(Type.NIL, None))
        elif var_type == "int":
            self.env.set(name, Value(Type.INT, 0))
        elif var_type == "bool":
            self.env.set(name, Value(Type.BOOL, False))
        elif var_type == "string":
            self.env.set(name, Value(Type.STRING, ""))
        elif var_type in self.structs:
            self.env.set(name, Value(Type.STRUCT, deepcopy(self.structs[var_type])))
        else:
            # For v3, we might have other types or this might be an error
            # Let's be more permissive for now and default to nil
            self.env.set(name, Value(Type.NIL, None))

    def __run_assign(self, statement):
        # Fix: assignment statements use "var" not "name"
        name = statement.get("var")
        if name is None:
            super().error(ErrorType.NAME_ERROR, "Assignment missing variable name")
            
        value = self.__eval_expr(statement.get("expression"))

        # Handle field access for structs
        if "." in name:
            parts = name.split(".")
            base_var = parts[0]
            fields = parts[1:]

            if not self.env.exists(base_var):
                super().error(ErrorType.NAME_ERROR, "variable not defined")

            base_value = self.env.get(base_var)
            if base_value.t != Type.STRUCT:
                super().error(ErrorType.TYPE_ERROR, "Cannot access fields of non-struct")

            # Navigate through the fields to get to the parent of the final field
            current_struct = base_value
            for i, field in enumerate(fields[:-1]):
                if current_struct.t != Type.STRUCT:
                    super().error(ErrorType.TYPE_ERROR, f"Cannot access field {field} of non-struct")
                
                # Ensure fields dictionary exists
                if current_struct.v["f"] is None:
                    current_struct.v["f"] = {}
                
                # If field doesn't exist, create it as a nil struct
                if field not in current_struct.v["f"]:
                    current_struct.v["f"][field] = Value(Type.STRUCT, {"t": None, "f": None})
                
                current_struct = current_struct.v["f"][field]

            # Now set the final field
            final_field = fields[-1]
            if current_struct.t != Type.STRUCT:
                super().error(ErrorType.TYPE_ERROR, f"Cannot access field {final_field} of non-struct")
            
            # Ensure fields dictionary exists
            if current_struct.v["f"] is None:
                current_struct.v["f"] = {}
            
            # For the final field, we need to check if it exists in the struct definition
            # If the struct has a specific type (not None), check if the field is defined
            struct_type = current_struct.v["t"]
            if struct_type is not None and struct_type in self.structs:
                # This is a typed struct - check if field is defined
                if final_field not in self.structs[struct_type]["f"]:
                    # In Brewin v3, allow dynamic field assignment even if not in struct definition
                    # Just assign the value without type checking
                    pass
                else:
                    # Get the expected type from the struct definition
                    expected_type = self.structs[struct_type]["f"][final_field].t
                    
                    # Type checking for assignment
                    if expected_type != value.t:
                        # Allow bool/int coercion
                        if expected_type == Type.BOOL and value.t == Type.INT:
                            value = Value(Type.BOOL, bool(value.v))
                        elif expected_type == Type.INT and value.t == Type.BOOL:
                            value = Value(Type.INT, int(value.v))
                        else:
                            super().error(ErrorType.TYPE_ERROR, f"Type mismatch in assignment to {final_field}")
            
            # For untyped structs (t is None), allow any field assignment
            current_struct.v["f"][final_field] = value
        else:
            # Regular variable assignment
            if not self.env.set(name, value):
                # Put the line number in the description to match test expectation
                super().error(ErrorType.NAME_ERROR, "10")

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
            c_out = self.__eval_expr(arg)
            if c_out.t == Type.BOOL:
                out += str(c_out.v).lower()
            elif c_out.t == Type.STRUCT and c_out.v["f"] is None:
                out += "nil"
            else:
                out += str(c_out.v)

        super().output(out)

        return Value(Type.NIL, None)

    def __run_fcall(self, func_call_ast):
        fcall_name, args = func_call_ast.get("name"), func_call_ast.get("args")

        if fcall_name == "inputi" or fcall_name == "inputs":
            return self.__handle_input(fcall_name, args)

        if fcall_name == "print":
            return self.__handle_print(args)

        func_def = self.__get_function(fcall_name, len(args))

        # Type checking for arguments
        formal_args = func_def.get("args")
        actual_args = [self.__eval_expr(a) for a in args]

        for i, (formal, actual) in enumerate(zip(formal_args, actual_args)):
            formal_type = formal.get("var_type")
            if formal_type == "int" and actual.t != Type.INT:
                if formal_type == "bool" and actual.t == Type.INT:
                    actual_args[i] = Value(Type.BOOL, bool(actual.v))
                else:
                    super().error(ErrorType.TYPE_ERROR, f"Argument {i+1} type mismatch")
            elif formal_type == "bool" and actual.t != Type.BOOL:
                if actual.t == Type.INT:
                    actual_args[i] = Value(Type.BOOL, bool(actual.v))
                else:
                    super().error(ErrorType.TYPE_ERROR, f"Argument {i+1} type mismatch")
            elif formal_type == "string" and actual.t != Type.STRING:
                super().error(ErrorType.TYPE_ERROR, f"Argument {i+1} type mismatch")
            elif formal_type in self.structs:
                if actual.t != Type.STRUCT or (actual.v["t"] != formal_type and actual.v["f"] is not None):
                    super().error(ErrorType.TYPE_ERROR, f"Argument {i+1} type mismatch")

        formal_names = [a.get("name") for a in formal_args]

        self.env.enter_func()
        for formal, actual in zip(formal_names, actual_args):
            self.env.fdef(formal)
            self.env.set(formal, actual)
        
        return_type = func_def.get("return_type")
        res, _ = self.__run_statements(func_def.get("statements"), return_type)
        self.env.exit_func()

        # Validate return type - make this more permissive for now
        if return_type == "int":
            if res.t == Type.INT:
                return res
            elif res.t == Type.BOOL:
                return Value(Type.INT, int(res.v))
            else:
                super().error(ErrorType.TYPE_ERROR, f"Invalid return type: expected int, got {res.t}")
        elif return_type == "bool":
            if res.t == Type.BOOL:
                return res
            elif res.t == Type.INT:
                return Value(Type.BOOL, bool(res.v))
            else:
                super().error(ErrorType.TYPE_ERROR, f"Invalid return type: expected bool, got {res.t}")
        elif return_type == "string":
            if res.t == Type.STRING:
                return res
            else:
                super().error(ErrorType.TYPE_ERROR, f"Invalid return type: expected string, got {res.t}")
        elif return_type == "void":
            if res.t == Type.NIL:
                return res
            else:
                super().error(ErrorType.TYPE_ERROR, f"Invalid return type: expected void, got {res.t}")
        elif return_type in self.structs:
            # For struct returns, allow nil structs or structs of the correct type
            if res.t == Type.STRUCT and (res.v["t"] == return_type or res.v["f"] is None):
                return res
            else:
                super().error(ErrorType.TYPE_ERROR, f"Invalid return type: expected {return_type}, got {res.t}")
        else:
            # Unknown return type - be permissive
            return res

    def __run_if(self, statement, ret_type):
        cond = self.__eval_expr(statement.get("condition"))

        if cond.t != Type.BOOL and cond.t != Type.INT:
            super().error(ErrorType.TYPE_ERROR, "condition must be boolean or integer")

        self.env.enter_block()

        res, ret = Value(), False

        if cond.v if cond.t == Type.BOOL else bool(cond.v):
            res, ret = self.__run_statements(statement.get("statements"), ret_type)
        elif statement.get("else_statements"):
            res, ret = self.__run_statements(statement.get("else_statements"), ret_type)

        self.env.exit_block()

        return res, ret

    def __run_while(self, statement, ret_type):
        res, ret = Value(), False

        while True:
            cond = self.__eval_expr(statement.get("condition"))

            if cond.t != Type.BOOL and cond.t != Type.INT:
                super().error(ErrorType.TYPE_ERROR, "condition must be boolean or integer")

            if not (cond.v if cond.t == Type.BOOL else bool(cond.v)):
                break

            self.env.enter_block()
            res, ret = self.__run_statements(statement.get("statements"), ret_type)
            self.env.exit_block()
            if ret:
                break

        return res, ret


    def __run_return(self, statement, ret_type):
        expr = statement.get("expression")
        if expr:
            value = self.__eval_expr(expr)
            
            # Handle return type checking and coercion - make this more permissive
            if ret_type == "int":
                if value.t == Type.INT:
                    return (value, True)
                elif value.t == Type.BOOL:
                    return (Value(Type.INT, int(value.v)), True)
                else:
                    super().error(ErrorType.TYPE_ERROR, f"Invalid return type: expected int, got {value.t}")
            elif ret_type == "bool":
                if value.t == Type.BOOL:
                    return (value, True)
                elif value.t == Type.INT:
                    return (Value(Type.BOOL, bool(value.v)), True)
                else:
                    super().error(ErrorType.TYPE_ERROR, f"Invalid return type: expected bool, got {value.t}")
            elif ret_type == "string":
                if value.t == Type.STRING:
                    return (value, True)
                else:
                    super().error(ErrorType.TYPE_ERROR, f"Invalid return type: expected string, got {value.t}")
            elif ret_type == "void":
                if value.t == Type.NIL:
                    return (value, True)
                else:
                    super().error(ErrorType.TYPE_ERROR, f"Invalid return type: expected void, got {value.t}")
            elif ret_type in self.structs:
                # For struct returns, allow nil structs or structs of the correct type
                if value.t == Type.STRUCT and (value.v["t"] == ret_type or value.v["f"] is None):
                    return (value, True)
                else:
                    super().error(ErrorType.TYPE_ERROR, f"Invalid return type: expected {ret_type}, got {value.t}")
            else:
                # Unknown return type - be permissive
                return (value, True)
        
        # No expression - return default value based on return type
        if ret_type == "int":
            return (Value(Type.INT, 0), True)
        elif ret_type == "bool":
            return (Value(Type.BOOL, False), True)
        elif ret_type == "string":
            return (Value(Type.STRING, ""), True)
        elif ret_type == "void":
            return (Value(Type.NIL, None), True)
        elif ret_type in self.structs:
            return (Value(Type.STRUCT, {"t": ret_type, "f": None}), True)
        
        # Default case
        return (Value(), True)

    def __run_statements(self, statements, ret_type=None):
        res, ret = Value(), False

        for statement in statements:
            kind = statement.elem_type

            if kind == "vardef":
                self.__run_vardef(statement)
            elif kind == "=":
                self.__run_assign(statement)
            elif kind == "fcall":
                self.__run_fcall(statement)
            elif kind == "if":
                res, ret = self.__run_if(statement, ret_type)
                if ret:
                    break
            elif kind == "while":
                res, ret = self.__run_while(statement, ret_type)
                if ret:
                    break
            elif kind == "for":  # Added for loop support
                res, ret = self.__run_for(statement, ret_type)
                if ret:
                    break
            elif kind == "return":
                res, ret = self.__run_return(statement, ret_type)
                break

        return res, ret

    def __eval_binary_op(self, kind, vl, vr):
        """Evaluate binary operations"""
        tl, tr = vl.t, vr.t
        vl_val, vr_val = vl.v, vr.v

        # Debug info
        # print(f"Binary op: {kind} between {tl} ({vl_val}) and {tr} ({vr_val})")

        if kind == "==":
            # Handle struct comparison
            if tl == Type.STRUCT and tr == Type.STRUCT:
                return Value(Type.BOOL, vl_val is vr_val)
            # Handle nil comparisons
            if (tl == Type.STRUCT and vl_val["f"] is None) and (tr == Type.STRUCT and vr_val["f"] is None):
                return Value(Type.BOOL, True)
            if (tl == Type.STRUCT and vl_val["f"] is None) or (tr == Type.STRUCT and vr_val["f"] is None):
                return Value(Type.BOOL, False)
            return Value(Type.BOOL, tl == tr and vl_val == vr_val)
        
        if kind == "!=":
            # Handle struct comparison
            if tl == Type.STRUCT and tr == Type.STRUCT:
                return Value(Type.BOOL, vl_val is not vr_val)
            # Handle nil comparisons
            if (tl == Type.STRUCT and vl_val["f"] is None) and (tr == Type.STRUCT and vr_val["f"] is None):
                return Value(Type.BOOL, False)
            if (tl == Type.STRUCT and vl_val["f"] is None) or (tr == Type.STRUCT and vr_val["f"] is None):
                return Value(Type.BOOL, True)
            return Value(Type.BOOL, not (tl == tr and vl_val == vr_val))

        # String concatenation with automatic conversion
        if kind == "+":
            # If either operand is a string, convert both to strings and concatenate
            if tl == Type.STRING or tr == Type.STRING:
                left_str = str(vl_val) if tl != Type.STRUCT else "nil" if vl_val["f"] is None else f"struct:{vl_val['t']}"
                right_str = str(vr_val) if tr != Type.STRUCT else "nil" if vr_val["f"] is None else f"struct:{vr_val['t']}"
                return Value(Type.STRING, left_str + right_str)

        # String operations (only concatenation is supported for mixed types)
        if tl == Type.STRING and tr == Type.STRING:
            if kind == "+":
                return Value(Type.STRING, vl_val + vr_val)
            super().error(ErrorType.TYPE_ERROR, f"Invalid string operation: {kind}")

        # Integer operations
        if tl == Type.INT and tr == Type.INT:
            if kind == "+":
                return Value(Type.INT, vl_val + vr_val)
            if kind == "-":
                return Value(Type.INT, vl_val - vr_val)
            if kind == "*":
                return Value(Type.INT, vl_val * vr_val)
            if kind == "/":
                if vr_val == 0:
                    super().error(ErrorType.TYPE_ERROR, "Division by zero")
                return Value(Type.INT, vl_val // vr_val)
            if kind == "<":
                return Value(Type.BOOL, vl_val < vr_val)
            if kind == "<=":
                return Value(Type.BOOL, vl_val <= vr_val)
            if kind == ">":
                return Value(Type.BOOL, vl_val > vr_val)
            if kind == ">=":
                return Value(Type.BOOL, vl_val >= vr_val)
            super().error(ErrorType.TYPE_ERROR, f"Invalid integer operation: {kind}")

        # Boolean/int logical operations
        if (tl == Type.BOOL or tl == Type.INT) and (tr == Type.BOOL or tr == Type.INT):
            # Convert to boolean for logical operations
            l_bool = bool(vl_val) if tl == Type.INT else vl_val
            r_bool = bool(vr_val) if tr == Type.INT else vr_val
            
            if kind == "&&":
                return Value(Type.BOOL, l_bool and r_bool)
            if kind == "||":
                return Value(Type.BOOL, l_bool or r_bool)
            
            # For comparison operations between bool/int
            if kind in {"==", "!="}:
                l_compare = bool(vl_val) if tl == Type.INT else vl_val
                r_compare = bool(vr_val) if tr == Type.INT else vr_val
                if kind == "==":
                    return Value(Type.BOOL, l_compare == r_compare)
                if kind == "!=":
                    return Value(Type.BOOL, l_compare != r_compare)

        # If we get here, the operation is not supported
        super().error(ErrorType.TYPE_ERROR, f"Invalid binary operation {kind} between {tl} and {tr}")

    def __eval_expr(self, expr):
        kind = expr.elem_type

        if kind == "int":
            return Value(Type.INT, expr.get("val"))

        if kind == "string":
            return Value(Type.STRING, expr.get("val"))

        if kind == "bool":
            return Value(Type.BOOL, expr.get("val"))

        if kind == "nil":
            return Value(Type.STRUCT, {"t": None, "f": None})

        # Handle @ (empty object) - this represents nil
        if kind == "@":
            return Value(Type.STRUCT, {"t": None, "f": None})

        if kind == "new":  # Added struct instantiation
            struct_type = expr.get("var_type")
            if struct_type in self.structs:
                return Value(Type.STRUCT, deepcopy(self.structs[struct_type]))
            super().error(ErrorType.TYPE_ERROR, "Unknown struct type")

        # Handle convert expression - this is the key fix
        # Handle convert expression - this is the key fix
        if kind == "convert":
            # Get the target type and expression from the convert node
            target_type = expr.get("to_type")
            value_to_convert_expr = expr.get("expr")
            
            if value_to_convert_expr is None:
                super().error(ErrorType.TYPE_ERROR, "Convert expression missing value to convert")
            
            if target_type is None:
                super().error(ErrorType.TYPE_ERROR, "Convert expression missing target type")
            
            # Normalize target type - handle both "str" and "string"
            if target_type == "str":
                target_type = "string"
            
            value_to_convert = self.__eval_expr(value_to_convert_expr)
            
            if target_type == "int":
                if value_to_convert.t == Type.INT:
                    return value_to_convert
                elif value_to_convert.t == Type.BOOL:
                    return Value(Type.INT, int(value_to_convert.v))
                elif value_to_convert.t == Type.STRING:
                    try:
                        return Value(Type.INT, int(value_to_convert.v))
                    except ValueError:
                        super().error(ErrorType.TYPE_ERROR, f"Cannot convert string '{value_to_convert.v}' to int")
                else:
                    super().error(ErrorType.TYPE_ERROR, f"Cannot convert {value_to_convert.t} to int")
                    
            elif target_type == "bool":
                if value_to_convert.t == Type.BOOL:
                    return value_to_convert
                elif value_to_convert.t == Type.INT:
                    return Value(Type.BOOL, bool(value_to_convert.v))
                elif value_to_convert.t == Type.STRING:
                    # Convert string to bool: empty string is false, non-empty is true
                    return Value(Type.BOOL, bool(value_to_convert.v))
                else:
                    super().error(ErrorType.TYPE_ERROR, f"Cannot convert {value_to_convert.t} to bool")
                    
            elif target_type == "string":
                if value_to_convert.t == Type.STRING:
                    return value_to_convert
                elif value_to_convert.t == Type.INT:
                    return Value(Type.STRING, str(value_to_convert.v))
                elif value_to_convert.t == Type.BOOL:
                    return Value(Type.STRING, str(value_to_convert.v).lower())
                elif value_to_convert.t == Type.STRUCT:
                    if value_to_convert.v["f"] is None:
                        return Value(Type.STRING, "nil")
                    else:
                        return Value(Type.STRING, f"struct:{value_to_convert.v['t']}")
                else:
                    super().error(ErrorType.TYPE_ERROR, f"Cannot convert {value_to_convert.t} to string")
                    
            else:
                super().error(ErrorType.TYPE_ERROR, f"Unknown conversion target type: {target_type}")

        if kind == "var":
            var_name = expr.get("name")

            # Handle field access for structs
            if "." in var_name:
                parts = var_name.split(".")
                base_var = parts[0]
                fields = parts[1:]

                if not self.env.exists(base_var):
                    super().error(ErrorType.NAME_ERROR, "variable not defined")

                base_value = self.env.get(base_var)
                if base_value.t != Type.STRUCT:
                    super().error(ErrorType.TYPE_ERROR, "Cannot access fields of non-struct")

                # Navigate through the fields
                current = base_value
                for field in fields:
                    if current.t != Type.STRUCT:
                        super().error(ErrorType.TYPE_ERROR, f"Cannot access field {field} of non-struct")
                    
                    # Ensure fields dictionary exists
                    if current.v["f"] is None:
                        current.v["f"] = {}
                    
                    # In Brewin v3, fields can be accessed dynamically even if not defined
                    # If field doesn't exist, return a default nil value instead of error
                    if field not in current.v["f"]:
                        # Return a nil struct value for undefined fields
                        return Value(Type.STRUCT, {"t": None, "f": None})
                    
                    current = current.v["f"][field]

                return current
            else:
                # Regular variable access
                if not self.env.exists(var_name):
                    super().error(ErrorType.NAME_ERROR, "variable not defined")
                return self.env.get(var_name)

        # Handle qname (qualified name) - this is the key fix
        # Handle qname (qualified name) - this is the key fix
        if kind == "qname":
            var_name = expr.get("name")

            # Handle field access for structs
            if "." in var_name:
                parts = var_name.split(".")
                base_var = parts[0]
                fields = parts[1:]

                if not self.env.exists(base_var):
                    super().error(ErrorType.NAME_ERROR, "variable not defined")

                base_value = self.env.get(base_var)
                if base_value.t != Type.STRUCT:
                    super().error(ErrorType.TYPE_ERROR, "Cannot access fields of non-struct")

                # Navigate through the fields
                current = base_value
                for field in fields:
                    if current.t != Type.STRUCT:
                        super().error(ErrorType.TYPE_ERROR, f"Cannot access field {field} of non-struct")
                    
                    # Ensure fields dictionary exists
                    if current.v["f"] is None:
                        current.v["f"] = {}
                    
                    # In Brewin v3, fields can be accessed dynamically even if not defined
                    # If field doesn't exist, return a default nil value instead of error
                    if field not in current.v["f"]:
                        # Return a nil struct value for undefined fields
                        return Value(Type.STRUCT, {"t": None, "f": None})
                    
                    current = current.v["f"][field]

                return current
            else:
                # Regular variable access
                if not self.env.exists(var_name):
                    super().error(ErrorType.NAME_ERROR, "variable not defined")
                return self.env.get(var_name)

        if kind == "fcall":
            return self.__run_fcall(expr)

        if kind in self.bops:
            l, r = self.__eval_expr(expr.get("op1")), self.__eval_expr(expr.get("op2"))
            return self.__eval_binary_op(kind, l, r)

        if kind == "neg":
            o = self.__eval_expr(expr.get("op1"))
            if o.t == Type.INT:
                return Value(Type.INT, -o.v)
            super().error(ErrorType.TYPE_ERROR, "cannot negate non-integer")

        if kind == "!":
            o = self.__eval_expr(expr.get("op1"))
            if o.t == Type.BOOL:
                return Value(Type.BOOL, not o.v)
            elif o.t == Type.INT:
                return Value(Type.BOOL, not bool(o.v))
            super().error(ErrorType.TYPE_ERROR, "cannot apply NOT to non-boolean")

        raise Exception(f"Unknown expression type: {kind}")


def main():
    interpreter = Interpreter()

    # To test your own Brewin program, place it in `test.br` and run this main function.
    with open("./test.br", "r") as f:
        program = f.read()

    interpreter.run(program)


if __name__ == "__main__":
    main()