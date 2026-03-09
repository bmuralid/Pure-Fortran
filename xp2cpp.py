import argparse
import ast
import difflib
import subprocess
import sys
import time
from pathlib import Path


class CppTranspiler:
    def __init__(self) -> None:
        self.indent_level = 0
        self.lines: list[str] = []
        self.function_return_types: dict[str, str] = {}
        self.function_local_types: dict[str, dict[str, str]] = {}
        self.function_arg_names: dict[str, list[str]] = {}
        self.function_arg_defaults: dict[str, dict[str, ast.AST]] = {}
        self.function_cpp_names: dict[str, str] = {}
        self.struct_fields: dict[str, dict[str, str]] = {}
        self.function_struct_types: dict[str, str] = {}

    def emit(self, line: str = "") -> None:
        self.lines.append("    " * self.indent_level + line)

    def transpile(self, tree: ast.Module) -> str:
        self.collect_function_types(tree)
        self.emit("#include <algorithm>")
        self.emit("#include <cctype>")
        self.emit("#include <cmath>")
        self.emit("#include <iomanip>")
        self.emit("#include <iostream>")
        self.emit("#include <map>")
        self.emit("#include <random>")
        self.emit("#include <sstream>")
        self.emit("#include <stdexcept>")
        self.emit("#include <string>")
        self.emit("#include <tuple>")
        self.emit("#include <vector>")
        self.emit('#include "pycpp_array.hpp"')
        self.emit()
        self.emit("std::string trim_copy(const std::string& value) {")
        self.indent_level += 1
        self.emit('const std::string whitespace = " \\t\\n\\r";')
        self.emit("const auto first = value.find_first_not_of(whitespace);")
        self.emit("if (first == std::string::npos) {")
        self.indent_level += 1
        self.emit('return "";')
        self.indent_level -= 1
        self.emit("}")
        self.emit("const auto last = value.find_last_not_of(whitespace);")
        self.emit("return value.substr(first, last - first + 1);")
        self.indent_level -= 1
        self.emit("}")
        self.emit()
        self.emit("std::string lower_copy(std::string value) {")
        self.indent_level += 1
        self.emit("std::transform(value.begin(), value.end(), value.begin(), [](unsigned char ch) {")
        self.indent_level += 1
        self.emit("return static_cast<char>(std::tolower(ch));")
        self.indent_level -= 1
        self.emit("});")
        self.emit("return value;")
        self.indent_level -= 1
        self.emit("}")
        self.emit()
        self.emit("std::string format_double(double value, const std::string& spec) {")
        self.indent_level += 1
        self.emit("std::ostringstream oss;")
        self.emit('if (spec.size() >= 3 && spec[0] == \'.\' && spec.back() == \'f\') {')
        self.indent_level += 1
        self.emit("const int precision = std::stoi(spec.substr(1, spec.size() - 2));")
        self.emit("oss << std::fixed << std::setprecision(precision) << value;")
        self.indent_level -= 1
        self.emit('} else if (spec.size() >= 3 && spec[0] == \'.\' && spec.back() == \'e\') {')
        self.indent_level += 1
        self.emit("const int precision = std::stoi(spec.substr(1, spec.size() - 2));")
        self.emit("oss << std::scientific << std::setprecision(precision) << value;")
        self.indent_level -= 1
        self.emit("} else {")
        self.indent_level += 1
        self.emit("oss << value;")
        self.indent_level -= 1
        self.emit("}")
        self.emit("return oss.str();")
        self.indent_level -= 1
        self.emit("}")
        self.emit()
        self.emit("std::mt19937 make_rng(int seed) {")
        self.indent_level += 1
        self.emit("return std::mt19937(seed);")
        self.indent_level -= 1
        self.emit("}")
        self.emit()
        self.emit("double normal_sample(std::mt19937& rng, double mean, double stddev) {")
        self.indent_level += 1
        self.emit("std::normal_distribution<double> dist(mean, stddev);")
        self.emit("return dist(rng);")
        self.indent_level -= 1
        self.emit("}")
        self.emit()
        self.emit("std::vector<double> concat_vectors(std::vector<double> left, const std::vector<double>& right) {")
        self.indent_level += 1
        self.emit("left.insert(left.end(), right.begin(), right.end());")
        self.emit("return left;")
        self.indent_level -= 1
        self.emit("}")
        self.emit()

        for struct_name, fields in self.struct_fields.items():
            self.emit(f"struct {struct_name} {{")
            self.indent_level += 1
            for field_name, field_type in fields.items():
                self.emit(f"{field_type} {field_name}{self.default_initializer(field_type)};")
            self.indent_level -= 1
            self.emit("};")
            self.emit()

        for node in tree.body:
            if isinstance(node, ast.FunctionDef):
                self.emit_function(node)
                self.emit()

        main_call = self.find_main_call(tree)
        self.emit("int main() {")
        self.indent_level += 1
        if main_call is not None:
            self.emit(f"{self.function_cpp_names.get(main_call, main_call)}();")
        self.emit("return 0;")
        self.indent_level -= 1
        self.emit("}")
        return "\n".join(self.lines) + "\n"

    def collect_function_types(self, tree: ast.Module) -> None:
        for node in tree.body:
            if not isinstance(node, ast.FunctionDef):
                continue
            self.function_cpp_names[node.name] = "py_main" if node.name == "main" else node.name
            arg_names = [arg.arg for arg in node.args.args]
            self.function_arg_names[node.name] = arg_names
            defaults: dict[str, ast.AST] = {}
            if node.args.defaults:
                offset = len(arg_names) - len(node.args.defaults)
                for name, default_node in zip(arg_names[offset:], node.args.defaults):
                    defaults[name] = default_node
            self.function_arg_defaults[node.name] = defaults
            local_types = {}
            for arg in node.args.args:
                local_types[arg.arg] = self.cpp_type_from_annotation(arg.annotation, arg.arg, defaults.get(arg.arg))
            for stmt in node.body:
                self.collect_stmt_types(stmt, local_types)
            for arg in node.args.args:
                if local_types.get(arg.arg) == "double":
                    inferred = self.infer_arg_type_from_usage(node, arg.arg, local_types)
                    if inferred != "double":
                        local_types[arg.arg] = inferred
            self.function_local_types[node.name] = local_types
            self.function_return_types[node.name] = self.infer_function_return_type(node, local_types)

    def infer_function_return_type(self, node: ast.FunctionDef, local_types: dict[str, str]) -> str:
        annotated = self.cpp_type_from_annotation(node.returns)
        if node.returns is not None:
            return annotated
        return_types: list[str] = []
        for inner in ast.walk(node):
            if isinstance(inner, ast.Return) and inner.value is not None:
                if isinstance(inner.value, ast.Dict):
                    struct_name = f"{self.function_cpp_names.get(node.name, node.name)}_result"
                    fields: dict[str, str] = {}
                    for key, value in zip(inner.value.keys, inner.value.values):
                        if not isinstance(key, ast.Constant) or not isinstance(key.value, str):
                            raise NotImplementedError("dict return keys must be string literals")
                        fields[key.value] = self.infer_expr_type(value, local_types)
                    self.struct_fields[struct_name] = fields
                    self.function_struct_types[node.name] = struct_name
                    return struct_name
                return_types.append(self.infer_expr_type(inner.value, local_types))
        if not return_types:
            return "void"
        unique_types: list[str] = []
        for item in return_types:
            if item not in unique_types:
                unique_types.append(item)
        if len(unique_types) == 1:
            return unique_types[0]
        if set(unique_types) == {"pycpp::Array1D<double>", "pycpp::Array2D<double>"}:
            return "pycpp::Array2D<double>"
        return unique_types[0]

    def collect_stmt_types(self, stmt: ast.stmt, local_types: dict[str, str]) -> None:
        if isinstance(stmt, ast.Assign) and len(stmt.targets) == 1 and isinstance(stmt.targets[0], ast.Name):
            name = stmt.targets[0].id
            inferred = self.infer_expr_type(stmt.value, local_types)
            current = local_types.get(name)
            if current is None or (current == "double" and inferred != "double"):
                local_types[name] = inferred
        elif isinstance(stmt, ast.Assign) and len(stmt.targets) == 1 and isinstance(stmt.targets[0], (ast.Tuple, ast.List)):
            target = stmt.targets[0]
            if isinstance(stmt.value, ast.Call):
                inferred = self.infer_expr_type(stmt.value, local_types)
                tuple_items = self.tuple_inner_types(inferred)
                if tuple_items and len(tuple_items) == len(target.elts):
                    for elt, item_type in zip(target.elts, tuple_items):
                        if isinstance(elt, ast.Name):
                            local_types[elt.id] = item_type
        elif isinstance(stmt, ast.If):
            for inner in stmt.body:
                self.collect_stmt_types(inner, local_types)
            for inner in stmt.orelse:
                self.collect_stmt_types(inner, local_types)
        elif isinstance(stmt, ast.For):
            if isinstance(stmt.target, ast.Name):
                local_types.setdefault(stmt.target.id, "int")
            for inner in stmt.body:
                self.collect_stmt_types(inner, local_types)
            for inner in stmt.orelse:
                self.collect_stmt_types(inner, local_types)

    def infer_expr_type(self, expr: ast.AST, local_types: dict[str, str]) -> str:
        if isinstance(expr, ast.Constant):
            if isinstance(expr.value, bool):
                return "bool"
            if isinstance(expr.value, int):
                return "int"
            if isinstance(expr.value, float):
                return "double"
            if isinstance(expr.value, str):
                return "std::string"
        if isinstance(expr, ast.Name):
            return local_types.get(expr.id, "double")
        if isinstance(expr, ast.Tuple):
            inner = ", ".join(self.infer_expr_type(elt, local_types) for elt in expr.elts)
            return f"std::tuple<{inner}>"
        if isinstance(expr, ast.Call):
            if isinstance(expr.func, ast.Name):
                if expr.func.id in {"abs", "float"}:
                    return "double"
                if expr.func.id == "int":
                    return "int"
                if expr.func.id == "len":
                    return "int"
                if expr.func.id in {"min", "max"} and len(expr.args) == 2:
                    left_type = self.infer_expr_type(expr.args[0], local_types)
                    right_type = self.infer_expr_type(expr.args[1], local_types)
                    if left_type == right_type == "int":
                        return "int"
                    return "double"
                if expr.func.id in {"print"}:
                    return "void"
                return self.function_return_types.get(expr.func.id, "double")
            if isinstance(expr.func, ast.Attribute):
                if self.is_string_method_call(expr):
                    return "std::string"
                if self.attr_chain(expr.func)[:2] == ["math", "erf"]:
                    return "double"
                if self.attr_chain(expr.func)[:2] == ["random", "Random"]:
                    return "std::mt19937"
                if self.attr_chain(expr.func)[:3] == ["np", "random", "default_rng"]:
                    return "std::mt19937"
                if expr.func.attr == "choice":
                    return "pycpp::Array1D<int>"
                if expr.func.attr == "normal":
                    return "pycpp::Array1D<double>"
                if self.attr_chain(expr.func)[:2] == ["np", "array"]:
                    return "pycpp::Array1D<double>"
                if self.attr_chain(expr.func)[:2] == ["np", "asarray"]:
                    return "pycpp::Array1D<double>"
                if self.attr_chain(expr.func)[:2] == ["np", "full"]:
                    if expr.args and isinstance(expr.args[0], ast.Tuple):
                        return "pycpp::Array2D<double>"
                    return "pycpp::Array1D<double>"
                if self.attr_chain(expr.func)[:2] == ["np", "empty"]:
                    if expr.args and isinstance(expr.args[0], ast.Tuple):
                        return "pycpp::Array2D<double>"
                    return "pycpp::Array1D<double>"
                if self.attr_chain(expr.func)[:2] == ["np", "maximum"]:
                    arg_type = self.infer_expr_type(expr.args[0], local_types) if expr.args else "double"
                    return arg_type if arg_type.startswith("pycpp::Array") else "pycpp::Array1D<double>"
                if self.attr_chain(expr.func)[:2] == ["np", "sqrt"]:
                    arg_type = self.infer_expr_type(expr.args[0], local_types) if expr.args else "double"
                    return arg_type if arg_type.startswith("pycpp::Array") else "pycpp::Array1D<double>"
                if self.attr_chain(expr.func)[:2] == ["np", "log"]:
                    arg_type = self.infer_expr_type(expr.args[0], local_types) if expr.args else "double"
                    return arg_type if arg_type.startswith("pycpp::Array") else "double"
                if self.attr_chain(expr.func)[:2] == ["np", "exp"]:
                    arg_type = self.infer_expr_type(expr.args[0], local_types) if expr.args else "double"
                    return arg_type if arg_type.startswith("pycpp::Array") else "double"
                if self.attr_chain(expr.func)[:2] == ["np", "sum"]:
                    if any(keyword.arg == "axis" for keyword in expr.keywords):
                        keepdims = next((kw.value for kw in expr.keywords if kw.arg == "keepdims"), None)
                        return "pycpp::Array2D<double>" if isinstance(keepdims, ast.Constant) and keepdims.value else "pycpp::Array1D<double>"
                    return "double"
                if self.attr_chain(expr.func)[:2] == ["np", "var"]:
                    return "double"
                if self.attr_chain(expr.func)[:2] == ["np", "argsort"]:
                    return "pycpp::Array1D<int>"
                if self.attr_chain(expr.func)[:2] == ["np", "max"]:
                    if any(keyword.arg == "axis" for keyword in expr.keywords):
                        keepdims = next((kw.value for kw in expr.keywords if kw.arg == "keepdims"), None)
                        return "pycpp::Array2D<double>" if isinstance(keepdims, ast.Constant) and keepdims.value else "pycpp::Array1D<double>"
                    return "double"
                if self.attr_chain(expr.func)[:2] == ["np", "squeeze"]:
                    return "pycpp::Array1D<double>"
                if expr.func.attr in {"sum", "copy", "ravel"}:
                    base_type = self.infer_expr_type(expr.func.value, local_types)
                    if expr.func.attr == "sum":
                        if any(keyword.arg == "axis" for keyword in expr.keywords):
                            return "pycpp::Array1D<double>"
                        return "double"
                    return base_type
                if expr.func.attr == "size":
                    return "int"
                if expr.func.attr == "gauss":
                    return "double"
        if isinstance(expr, ast.BinOp):
            if (
                isinstance(expr.op, ast.Mult)
                and isinstance(expr.left, ast.List)
                and len(expr.left.elts) == 1
            ):
                return "std::vector<double>"
            left_type = self.infer_expr_type(expr.left, local_types)
            right_type = self.infer_expr_type(expr.right, local_types)
            if left_type.startswith("pycpp::Array") and right_type in {"double", "int"}:
                return left_type
            if right_type.startswith("pycpp::Array") and left_type in {"double", "int"}:
                return right_type
            if (
                isinstance(expr.op, ast.Add)
                and left_type == "std::vector<double>"
                and right_type == "std::vector<double>"
            ):
                return "std::vector<double>"
            if left_type.startswith("pycpp::Array") and right_type.startswith("pycpp::Array"):
                return left_type
            if left_type == right_type == "int":
                return "int"
            return "double"
        if isinstance(expr, ast.UnaryOp):
            return self.infer_expr_type(expr.operand, local_types)
        if isinstance(expr, ast.Compare):
            return "bool"
        if isinstance(expr, ast.BoolOp):
            return "bool"
        if isinstance(expr, ast.Dict):
            return "std::map<std::string, double>"
        if isinstance(expr, ast.Subscript):
            value_type = self.infer_expr_type(expr.value, local_types)
            if value_type in self.struct_fields and isinstance(expr.slice, ast.Constant) and isinstance(expr.slice.value, str):
                return self.struct_fields[value_type][expr.slice.value]
            if isinstance(expr.slice, ast.Tuple) and value_type == "pycpp::Array1D<double>":
                return "pycpp::Array2D<double>"
            if value_type == "std::vector<double>":
                return "double"
            if value_type == "pycpp::Array1D<double>":
                index_type = self.infer_expr_type(expr.slice, local_types)
                if index_type == "pycpp::Array1D<int>":
                    return "pycpp::Array1D<double>"
                return "double"
            if value_type == "pycpp::Array1D<int>":
                index_type = self.infer_expr_type(expr.slice, local_types)
                if index_type == "pycpp::Array1D<int>":
                    return "pycpp::Array1D<int>"
                return "int"
            if value_type == "pycpp::Array2D<double>":
                return "pycpp::Array2D<double>"
            return "double"
        if isinstance(expr, ast.List):
            return "std::vector<double>"
        if isinstance(expr, ast.ListComp):
            return "std::vector<double>"
        return "double"

    def cpp_type_from_annotation(self, annotation: ast.AST | None, arg_name: str | None = None, default_node: ast.AST | None = None) -> str:
        if annotation is None:
            if arg_name == "axis":
                return "int"
            if arg_name in {"keepdims", "replace"}:
                return "bool"
            if isinstance(default_node, ast.Constant):
                if isinstance(default_node.value, bool):
                    return "bool"
                if isinstance(default_node.value, int):
                    return "int"
                if isinstance(default_node.value, float):
                    return "double"
            return "double"
        if isinstance(annotation, ast.Constant) and annotation.value is None:
            return "void"
        if isinstance(annotation, ast.Name):
            if annotation.id == "float":
                return "double"
            if annotation.id == "int":
                return "int"
            if annotation.id == "str":
                return "std::string"
            if annotation.id == "bool":
                return "bool"
            if annotation.id == "None":
                return "void"
        if isinstance(annotation, ast.Subscript):
            base = annotation.value
            if isinstance(base, ast.Name) and base.id == "dict":
                return "std::map<std::string, double>"
            if isinstance(base, ast.Name) and base.id == "tuple":
                sub = annotation.slice
                if isinstance(sub, ast.Tuple):
                    inner = ", ".join(self.cpp_type_from_annotation(elt) for elt in sub.elts)
                    return f"std::tuple<{inner}>"
            if isinstance(base, ast.Name) and base.id == "list":
                return "std::vector<double>"
        return "double"

    def emit_function(self, node: ast.FunctionDef) -> None:
        return_type = self.function_return_types[node.name]
        local_types = self.function_local_types[node.name]
        args = []
        for arg in node.args.args:
            args.append(f"{local_types.get(arg.arg, self.cpp_type_from_annotation(arg.annotation, arg.arg, self.function_arg_defaults[node.name].get(arg.arg)))} {arg.arg}")
        cpp_name = self.function_cpp_names.get(node.name, node.name)
        self.emit(f"{return_type} {cpp_name}({', '.join(args)}) {{")
        self.indent_level += 1
        declared = set(arg.arg for arg in node.args.args)
        for name, cpp_type in local_types.items():
            if name in declared:
                continue
            self.emit(f"{cpp_type} {name}{self.default_initializer(cpp_type)};")
            declared.add(name)
        for stmt in node.body:
            if self.is_docstring_stmt(stmt):
                continue
            self.emit_stmt(stmt, local_types, declared)
        self.indent_level -= 1
        self.emit("}")

    def emit_stmt(self, stmt: ast.stmt, local_types: dict[str, str], declared: set[str]) -> None:
        if isinstance(stmt, ast.Assign):
            self.emit_assign(stmt, local_types, declared)
            return
        if isinstance(stmt, ast.AugAssign):
            self.emit_augassign(stmt, local_types, declared)
            return
        if isinstance(stmt, ast.Return):
            if stmt.value is None:
                self.emit("return;")
            else:
                if isinstance(stmt.value, ast.Dict):
                    struct_type = self.function_return_types.get(self.current_function_name(local_types), "")
                    if struct_type in self.struct_fields:
                        values = [self.expr(value, local_types) for value in stmt.value.values]
                        self.emit(f"return {struct_type}{{{', '.join(values)}}};")
                    else:
                        self.emit(f"return {self.expr(stmt.value, local_types)};")
                else:
                    self.emit(f"return {self.expr(stmt.value, local_types)};")
            return
        if isinstance(stmt, ast.If):
            self.emit(f"if ({self.expr(stmt.test, local_types)}) {{")
            self.indent_level += 1
            for inner in stmt.body:
                self.emit_stmt(inner, local_types, declared)
            self.indent_level -= 1
            if stmt.orelse:
                self.emit("} else {")
                self.indent_level += 1
                for inner in stmt.orelse:
                    self.emit_stmt(inner, local_types, declared)
                self.indent_level -= 1
            self.emit("}")
            return
        if isinstance(stmt, ast.Raise):
            message = "runtime error"
            if isinstance(stmt.exc, ast.Call) and stmt.exc.args:
                message = self.string_literal_value(stmt.exc.args[0])
            self.emit(f'throw std::runtime_error("{self.escape_cpp_string(message)}");')
            return
        if isinstance(stmt, ast.Expr) and isinstance(stmt.value, ast.Call):
            if isinstance(stmt.value.func, ast.Name) and stmt.value.func.id == "print":
                self.emit_print(stmt.value, local_types)
                return
            if isinstance(stmt.value.func, ast.Attribute) and stmt.value.func.attr == "append":
                self.emit_append(stmt.value, local_types)
                return
            self.emit(f"{self.expr(stmt.value, local_types)};")
            return
        if isinstance(stmt, ast.For):
            self.emit_for(stmt, local_types, declared)
            return
        if isinstance(stmt, ast.Break):
            self.emit("break;")
            return
        raise NotImplementedError(f"unsupported statement: {ast.dump(stmt)}")

    def emit_assign(self, stmt: ast.Assign, local_types: dict[str, str], declared: set[str]) -> None:
        target = stmt.targets[0]
        if isinstance(target, ast.Subscript):
            target_expr = self.subscript_expr(target, local_types)
            value = self.expr(stmt.value, local_types)
            self.emit(f"{target_expr} = {value};")
            return
        if isinstance(target, (ast.Tuple, ast.List)):
            if not isinstance(stmt.value, ast.Call):
                raise NotImplementedError("tuple assignment requires call on right-hand side")
            names = []
            for elt in target.elts:
                if not isinstance(elt, ast.Name):
                    raise NotImplementedError("only simple tuple targets are supported")
                names.append(elt.id)
            for name in names:
                if name not in declared:
                    cpp_type = local_types.get(name, "double")
                    self.emit(f"{cpp_type} {name};")
                    declared.add(name)
            tie_targets = ", ".join(names)
            self.emit(f"std::tie({tie_targets}) = {self.expr(stmt.value, local_types)};")
            return
        if not isinstance(target, ast.Name):
            raise NotImplementedError("only simple assignments are supported")
        name = target.id
        value = self.expr(stmt.value, local_types)
        if name in declared:
            target_type = local_types.get(name, "double")
            value_type = self.infer_expr_type(stmt.value, local_types)
            if target_type == "pycpp::Array2D<double>" and value_type == "pycpp::Array1D<double>":
                axis = self.squeeze_axis_expr(stmt.value, local_types)
                self.emit(f"{name} = pycpp::unsqueeze({value}, {axis});")
            else:
                self.emit(f"{name} = {value};")
        else:
            cpp_type = local_types.get(name, self.infer_expr_type(stmt.value, local_types))
            self.emit(f"{cpp_type} {name} = {value};")
            declared.add(name)

    def emit_augassign(self, stmt: ast.AugAssign, local_types: dict[str, str], declared: set[str]) -> None:
        if not isinstance(stmt.target, ast.Name):
            raise NotImplementedError("only simple augmented assignments are supported")
        op = self.binop(stmt.op)
        self.emit(f"{stmt.target.id} {op}= {self.expr(stmt.value, local_types)};")

    def emit_for(self, stmt: ast.For, local_types: dict[str, str], declared: set[str]) -> None:
        if (
            isinstance(stmt.target, ast.Tuple)
            and len(stmt.target.elts) == 2
            and all(isinstance(elt, ast.Name) for elt in stmt.target.elts)
            and isinstance(stmt.iter, ast.Call)
            and isinstance(stmt.iter.func, ast.Name)
            and stmt.iter.func.id == "enumerate"
            and len(stmt.iter.args) == 1
        ):
            index_name = stmt.target.elts[0].id
            value_name = stmt.target.elts[1].id
            vector_expr = self.expr(stmt.iter.args[0], local_types)
            if index_name not in declared:
                self.emit(f"for (int {index_name} = 0; {index_name} < static_cast<int>({vector_expr}.size()); ++{index_name}) {{")
                declared.add(index_name)
            else:
                self.emit(f"for ({index_name} = 0; {index_name} < static_cast<int>({vector_expr}.size()); ++{index_name}) {{")
            self.indent_level += 1
            value_type = local_types.get(value_name, "double")
            if value_name not in declared:
                self.emit(f"{value_type} {value_name} = {vector_expr}[{index_name}];")
                declared.add(value_name)
            else:
                self.emit(f"{value_name} = {vector_expr}[{index_name}];")
            for inner in stmt.body:
                self.emit_stmt(inner, local_types, declared)
            self.indent_level -= 1
            self.emit("}")
            return

        if not isinstance(stmt.target, ast.Name):
            raise NotImplementedError("only simple for-loop targets are supported")
        if not (
            isinstance(stmt.iter, ast.Call)
            and isinstance(stmt.iter.func, ast.Name)
            and stmt.iter.func.id == "range"
        ):
            raise NotImplementedError("only for ... in range(...) is supported")
        loop_var = stmt.target.id
        if len(stmt.iter.args) == 1:
            start_expr = "0"
            stop_expr = self.expr(stmt.iter.args[0], local_types)
            step_expr = "1"
        elif len(stmt.iter.args) == 2:
            start_expr = self.expr(stmt.iter.args[0], local_types)
            stop_expr = self.expr(stmt.iter.args[1], local_types)
            step_expr = "1"
        elif len(stmt.iter.args) == 3:
            start_expr = self.expr(stmt.iter.args[0], local_types)
            stop_expr = self.expr(stmt.iter.args[1], local_types)
            step_expr = self.expr(stmt.iter.args[2], local_types)
        else:
            raise NotImplementedError("range supports one, two, or three arguments only")
        condition = f"{loop_var} < {stop_expr}"
        if len(stmt.iter.args) == 3 and self.is_negative_expr(stmt.iter.args[2]):
            condition = f"{loop_var} > {stop_expr}"
        if loop_var not in declared:
            declared.add(loop_var)
            self.emit(f"for (int {loop_var} = {start_expr}; {condition}; {loop_var} += {step_expr}) {{")
        else:
            self.emit(f"for ({loop_var} = {start_expr}; {condition}; {loop_var} += {step_expr}) {{")
        self.indent_level += 1
        for inner in stmt.body:
            self.emit_stmt(inner, local_types, declared)
        self.indent_level -= 1
        self.emit("}")

    def emit_print(self, call: ast.Call, local_types: dict[str, str]) -> None:
        if len(call.args) == 0:
            self.emit('std::cout << "\\n";')
            return
        if len(call.args) > 1:
            pieces = [self.expr(arg, local_types) for arg in call.args]
            self.emit("std::cout << " + ' << " " << '.join(pieces) + ' << "\\n";')
            return
        expr = call.args[0]
        if isinstance(expr, ast.JoinedStr):
            parts = []
            for value in expr.values:
                if isinstance(value, ast.Constant) and isinstance(value.value, str):
                    parts.append(f'"{self.escape_cpp_string(value.value)}"')
                elif isinstance(value, ast.FormattedValue):
                    fmt = ""
                    if value.format_spec is not None:
                        fmt = self.joined_str_literal(value.format_spec)
                    inner = self.expr(value.value, local_types)
                    if fmt:
                        parts.append(f'format_double({inner}, "{self.escape_cpp_string(fmt)}")')
                    else:
                        parts.append(f"({inner})")
                else:
                    raise NotImplementedError("unsupported f-string component")
            self.emit("std::cout << " + " << ".join(parts) + ' << "\\n";')
            return
        self.emit(f"std::cout << {self.expr(expr, local_types)} << \"\\n\";")

    def emit_append(self, call: ast.Call, local_types: dict[str, str]) -> None:
        if len(call.args) != 1 or not isinstance(call.func, ast.Attribute):
            raise NotImplementedError("append requires one argument")
        target = self.expr(call.func.value, local_types)
        value = self.expr(call.args[0], local_types)
        self.emit(f"{target}.push_back({value});")

    def expr(self, node: ast.AST, local_types: dict[str, str]) -> str:
        if isinstance(node, ast.Constant):
            if isinstance(node.value, str):
                return f'"{self.escape_cpp_string(node.value)}"'
            if isinstance(node.value, bool):
                return "true" if node.value else "false"
            if node.value is None:
                return "nullptr"
            return repr(node.value)
        if isinstance(node, ast.Name):
            return node.id
        if isinstance(node, ast.BinOp):
            if (
                isinstance(node.op, ast.Mult)
                and isinstance(node.left, ast.List)
                and len(node.left.elts) == 1
            ):
                fill_value = self.expr(node.left.elts[0], local_types)
                count_expr = self.expr(node.right, local_types)
                return f"std::vector<double>({count_expr}, {fill_value})"
            if (
                isinstance(node.op, ast.Add)
                and self.infer_expr_type(node.left, local_types) == "std::vector<double>"
                and self.infer_expr_type(node.right, local_types) == "std::vector<double>"
            ):
                left_expr = self.expr(node.left, local_types)
                right_expr = self.expr(node.right, local_types)
                return f"concat_vectors({left_expr}, {right_expr})"
            if isinstance(node.op, ast.Pow):
                left_type = self.infer_expr_type(node.left, local_types)
                if left_type.startswith("pycpp::Array"):
                    return f"pycpp::pow({self.expr(node.left, local_types)}, {self.expr(node.right, local_types)})"
                return f"std::pow({self.expr(node.left, local_types)}, {self.expr(node.right, local_types)})"
            op = self.binop(node.op)
            return f"({self.expr(node.left, local_types)} {op} {self.expr(node.right, local_types)})"
        if isinstance(node, ast.UnaryOp):
            if isinstance(node.op, ast.UAdd):
                return f"(+{self.expr(node.operand, local_types)})"
            if isinstance(node.op, ast.USub):
                return f"(-{self.expr(node.operand, local_types)})"
            if isinstance(node.op, ast.Not):
                return f"(!{self.expr(node.operand, local_types)})"
            raise NotImplementedError(f"unsupported unary operator: {type(node.op).__name__}")
        if isinstance(node, ast.Compare):
            return self.compare_expr(node, local_types)
        if isinstance(node, ast.BoolOp):
            op = "&&" if isinstance(node.op, ast.And) else "||"
            return "(" + f" {op} ".join(self.expr(v, local_types) for v in node.values) + ")"
        if isinstance(node, ast.IfExp):
            return (
                "("
                + self.expr(node.test, local_types)
                + " ? "
                + self.expr(node.body, local_types)
                + " : "
                + self.expr(node.orelse, local_types)
                + ")"
            )
        if isinstance(node, ast.Call):
            return self.call_expr(node, local_types)
        if isinstance(node, ast.Tuple):
            return "std::make_tuple(" + ", ".join(self.expr(elt, local_types) for elt in node.elts) + ")"
        if isinstance(node, ast.List):
            return "std::vector<double>{" + ", ".join(self.expr(elt, local_types) for elt in node.elts) + "}"
        if isinstance(node, ast.ListComp):
            return self.list_comp_expr(node, local_types)
        if isinstance(node, ast.Dict):
            items = []
            for key, value in zip(node.keys, node.values):
                items.append("{" + f"{self.expr(key, local_types)}, {self.expr(value, local_types)}" + "}")
            return "std::map<std::string, double>{" + ", ".join(items) + "}"
        if isinstance(node, ast.Subscript):
            return self.subscript_expr(node, local_types)
        if isinstance(node, ast.Set):
            raise NotImplementedError("set literals are only supported in membership tests")
        if isinstance(node, ast.Attribute):
            chain = self.attr_chain(node)
            if chain == ["math", "pi"]:
                return "3.14159265358979323846"
            if chain == ["np", "pi"]:
                return "3.14159265358979323846"
            if node.attr == "size":
                base_type = self.infer_expr_type(node.value, local_types)
                if base_type in {"pycpp::Array1D<double>", "pycpp::Array1D<int>", "pycpp::Array2D<double>"}:
                    return f"static_cast<int>({self.expr(node.value, local_types)}.size())"
        raise NotImplementedError(f"unsupported expression: {ast.dump(node)}")

    def call_expr(self, node: ast.Call, local_types: dict[str, str]) -> str:
        if self.is_string_method_call(node):
            return self.translate_string_method_call(node, local_types)

        if isinstance(node.func, ast.Name):
            name = node.func.id
            if name == "abs":
                return f"std::abs({self.expr(node.args[0], local_types)})"
            if name == "len":
                return f"static_cast<int>({self.expr(node.args[0], local_types)}.size())"
            if name in {"float", "int"}:
                target = "double" if name == "float" else "int"
                return f"static_cast<{target}>({self.expr(node.args[0], local_types)})"
            if name == "max":
                return f"std::max({self.expr(node.args[0], local_types)}, {self.expr(node.args[1], local_types)})"
            if name == "min":
                return f"std::min({self.expr(node.args[0], local_types)}, {self.expr(node.args[1], local_types)})"
            args = ", ".join(self.call_args(node, local_types))
            cpp_name = self.function_cpp_names.get(name, name)
            return f"{cpp_name}({args})"

        if isinstance(node.func, ast.Attribute):
            if (
                isinstance(node.func.value, ast.Name)
                and node.func.value.id == "random"
                and node.func.attr == "Random"
            ):
                return f"make_rng({self.expr(node.args[0], local_types)})"
            if self.attr_chain(node.func)[:3] == ["np", "random", "default_rng"]:
                return f"make_rng({self.expr(node.args[0], local_types)})"
            chain = self.attr_chain(node.func)
            if chain[:1] == ["math"]:
                cpp_name = {
                    "erf": "std::erf",
                    "sqrt": "std::sqrt",
                    "log": "std::log",
                    "exp": "std::exp",
                    "floor": "std::floor",
                }.get(chain[1])
                if cpp_name is None:
                    raise NotImplementedError(f"unsupported math function: {'.'.join(chain)}")
                args = ", ".join(self.expr(arg, local_types) for arg in node.args)
                return f"{cpp_name}({args})"
            if chain[:2] == ["np", "array"]:
                if len(node.args) != 1:
                    raise NotImplementedError("np.array currently requires one positional argument")
                if isinstance(node.args[0], ast.List):
                    inner = ", ".join(self.expr(elt, local_types) for elt in node.args[0].elts)
                    return f"pycpp::array<double>({{{inner}}})"
                return f"pycpp::asarray({self.expr(node.args[0], local_types)})"
            if chain[:2] == ["np", "asarray"]:
                return f"pycpp::asarray({self.expr(node.args[0], local_types)})"
            if chain[:2] == ["np", "full"]:
                if len(node.args) >= 2 and isinstance(node.args[0], ast.Tuple) and len(node.args[0].elts) == 2:
                    return (
                        f"pycpp::full<double>({self.expr(node.args[0].elts[0], local_types)}, "
                        f"{self.expr(node.args[0].elts[1], local_types)}, {self.expr(node.args[1], local_types)})"
                    )
                return f"pycpp::full<double>({self.expr(node.args[0], local_types)}, {self.expr(node.args[1], local_types)})"
            if chain[:2] == ["np", "empty"]:
                if node.args and isinstance(node.args[0], ast.Tuple) and len(node.args[0].elts) == 2:
                    return (
                        f"pycpp::empty<double>({self.expr(node.args[0].elts[0], local_types)}, "
                        f"{self.expr(node.args[0].elts[1], local_types)})"
                    )
                return f"pycpp::empty<double>({self.expr(node.args[0], local_types)})"
            if chain[:2] == ["np", "maximum"]:
                return f"pycpp::maximum({self.expr(node.args[0], local_types)}, {self.expr(node.args[1], local_types)})"
            if chain[:2] == ["np", "sqrt"]:
                return f"pycpp::sqrt({self.expr(node.args[0], local_types)})"
            if chain[:2] == ["np", "log"]:
                arg = self.expr(node.args[0], local_types)
                arg_type = self.infer_expr_type(node.args[0], local_types)
                if arg_type.startswith("pycpp::Array"):
                    return f"pycpp::log({arg})"
                return f"std::log({arg})"
            if chain[:2] == ["np", "exp"]:
                arg = self.expr(node.args[0], local_types)
                arg_type = self.infer_expr_type(node.args[0], local_types)
                if arg_type.startswith("pycpp::Array"):
                    return f"pycpp::exp({arg})"
                return f"std::exp({arg})"
            if chain[:2] == ["np", "sum"]:
                axis_kw = next((kw for kw in node.keywords if kw.arg == "axis"), None)
                keepdims_kw = next((kw for kw in node.keywords if kw.arg == "keepdims"), None)
                if axis_kw is not None:
                    if keepdims_kw is not None and isinstance(keepdims_kw.value, ast.Constant) and keepdims_kw.value.value:
                        return f"pycpp::sum_axis_keepdims({self.expr(node.args[0], local_types)}, {self.expr(axis_kw.value, local_types)})"
                    return f"pycpp::sum_axis({self.expr(node.args[0], local_types)}, {self.expr(axis_kw.value, local_types)})"
                return f"pycpp::sum({self.expr(node.args[0], local_types)})"
            if chain[:2] == ["np", "var"]:
                return f"pycpp::var({self.expr(node.args[0], local_types)})"
            if chain[:2] == ["np", "argsort"]:
                return f"pycpp::argsort({self.expr(node.args[0], local_types)})"
            if chain[:2] == ["np", "max"]:
                axis_kw = next((kw for kw in node.keywords if kw.arg == "axis"), None)
                keepdims_kw = next((kw for kw in node.keywords if kw.arg == "keepdims"), None)
                if axis_kw is not None:
                    if keepdims_kw is not None and isinstance(keepdims_kw.value, ast.Constant) and keepdims_kw.value.value:
                        return f"pycpp::max_axis_keepdims({self.expr(node.args[0], local_types)}, {self.expr(axis_kw.value, local_types)})"
                    return f"pycpp::max_axis({self.expr(node.args[0], local_types)}, {self.expr(axis_kw.value, local_types)})"
                return f"pycpp::max_axis({self.expr(node.args[0], local_types)}, 0)"
            if chain[:2] == ["np", "squeeze"]:
                axis_kw = next((kw for kw in node.keywords if kw.arg == "axis"), None)
                if axis_kw is None:
                    raise NotImplementedError("np.squeeze currently requires axis=...")
                return f"pycpp::squeeze({self.expr(node.args[0], local_types)}, {self.expr(axis_kw.value, local_types)})"
            if node.func.attr == "sum" and not node.args:
                axis_kw = next((kw for kw in node.keywords if kw.arg == "axis"), None)
                if axis_kw is not None:
                    return f"pycpp::sum_axis({self.expr(node.func.value, local_types)}, {self.expr(axis_kw.value, local_types)})"
                return f"{self.expr(node.func.value, local_types)}.sum()"
            if node.func.attr == "copy" and not node.args:
                return f"{self.expr(node.func.value, local_types)}.copy()"
            if node.func.attr == "ravel" and not node.args:
                return f"{self.expr(node.func.value, local_types)}.ravel()"
            if node.func.attr == "choice":
                size_arg = None
                p_arg = None
                replace_arg = None
                for keyword in node.keywords:
                    if keyword.arg == "size":
                        size_arg = self.expr(keyword.value, local_types)
                    elif keyword.arg == "p":
                        p_arg = self.expr(keyword.value, local_types)
                    elif keyword.arg == "replace":
                        replace_arg = self.expr(keyword.value, local_types)
                replace_text = replace_arg if replace_arg is not None else "true"
                if len(node.args) != 1 or size_arg is None:
                    raise NotImplementedError("rng.choice currently requires one positional argument and size=...")
                first_type = self.infer_expr_type(node.args[0], local_types)
                if p_arg is not None:
                    return (
                        "pycpp::choice("
                        + self.expr(node.func.value, local_types)
                        + ", "
                        + self.expr(node.args[0], local_types)
                        + ", "
                        + size_arg
                        + ", "
                        + p_arg
                        + ")"
                    )
                if first_type == "pycpp::Array1D<double>":
                    return (
                        "pycpp::choice("
                        + self.expr(node.func.value, local_types)
                        + ", "
                        + self.expr(node.args[0], local_types)
                        + ", "
                        + size_arg
                        + ", "
                        + replace_text
                        + ")"
                    )
                raise NotImplementedError("rng.choice currently supports weighted integer choice or Array1D<double> choice")
            if node.func.attr == "normal":
                loc_arg = None
                scale_arg = None
                size_arg = None
                for keyword in node.keywords:
                    if keyword.arg == "loc":
                        loc_arg = self.expr(keyword.value, local_types)
                    elif keyword.arg == "scale":
                        scale_arg = self.expr(keyword.value, local_types)
                    elif keyword.arg == "size":
                        size_arg = self.expr(keyword.value, local_types)
                if loc_arg is None or scale_arg is None or size_arg is None:
                    raise NotImplementedError("rng.normal currently requires loc=..., scale=..., size=...")
                return (
                    "pycpp::normal("
                    + self.expr(node.func.value, local_types)
                    + ", "
                    + loc_arg
                    + ", "
                    + scale_arg
                    + ", "
                    + size_arg
                    + ")"
                )
            if node.func.attr == "gauss":
                args = ", ".join(self.expr(arg, local_types) for arg in node.args)
                return f"normal_sample({self.expr(node.func.value, local_types)}, {args})"

        raise NotImplementedError(f"unsupported call: {ast.dump(node)}")

    def subscript_expr(self, node: ast.Subscript, local_types: dict[str, str]) -> str:
        value_type = self.infer_expr_type(node.value, local_types)
        if isinstance(node.slice, ast.Constant) and isinstance(node.slice.value, str):
            if value_type in self.struct_fields:
                return f"{self.expr(node.value, local_types)}.{node.slice.value}"
            return f"{self.expr(node.value, local_types)}.at({self.expr(node.slice, local_types)})"
        slice_type = self.infer_expr_type(node.slice, local_types)
        if isinstance(node.slice, ast.Tuple) and len(node.slice.elts) == 2:
            first, second = node.slice.elts
            if (
                value_type == "pycpp::Array1D<double>"
                and isinstance(first, ast.Slice)
                and first.lower is None and first.upper is None
                and isinstance(second, ast.Constant) and second.value is None
            ):
                return f"pycpp::col_vector({self.expr(node.value, local_types)})"
            if (
                value_type == "pycpp::Array1D<double>"
                and isinstance(second, ast.Slice)
                and second.lower is None and second.upper is None
                and isinstance(first, ast.Constant) and first.value is None
            ):
                return f"pycpp::row_vector({self.expr(node.value, local_types)})"
            if (
                value_type == "pycpp::Array2D<double>"
                and all(isinstance(elt, ast.Slice) and elt.lower is None and elt.upper is None for elt in node.slice.elts)
            ):
                return self.expr(node.value, local_types)
            raise NotImplementedError("unsupported tuple subscript pattern")
        if value_type in {"pycpp::Array1D<double>", "pycpp::Array1D<int>"} and slice_type == "pycpp::Array1D<int>":
            return f"pycpp::take({self.expr(node.value, local_types)}, {self.expr(node.slice, local_types)})"
        if value_type in {"pycpp::Array1D<double>", "pycpp::Array1D<int>"} and isinstance(node.slice, ast.Slice):
            lower = self.expr(node.slice.lower, local_types) if node.slice.lower is not None else "0"
            upper = self.expr(node.slice.upper, local_types) if node.slice.upper is not None else f"static_cast<int>({self.expr(node.value, local_types)}.size())"
            return f"pycpp::slice({self.expr(node.value, local_types)}, {lower}, {upper})"
        index_expr = self.expr(node.slice, local_types)
        if isinstance(node.slice, ast.UnaryOp) and isinstance(node.slice.op, ast.USub):
            base_expr = self.expr(node.value, local_types)
            index_expr = f"(static_cast<int>({base_expr}.size()) + {index_expr})"
            return f"{base_expr}[{index_expr}]"
        return f"{self.expr(node.value, local_types)}[{index_expr}]"

    def call_args(self, node: ast.Call, local_types: dict[str, str]) -> list[str]:
        args = [self.expr(arg, local_types) for arg in node.args]
        if not node.keywords:
            if isinstance(node.func, ast.Name):
                arg_names = self.function_arg_names.get(node.func.id)
                defaults = self.function_arg_defaults.get(node.func.id, {})
                if arg_names is not None and len(args) < len(arg_names):
                    values_by_name = {name: value for name, value in zip(arg_names, args)}
                    for name in arg_names[len(args):]:
                        if name in defaults:
                            values_by_name[name] = self.expr(defaults[name], local_types)
                    return [values_by_name[name] for name in arg_names]
            return args
        if not isinstance(node.func, ast.Name):
            raise NotImplementedError("keyword arguments are only supported for simple function calls")
        arg_names = self.function_arg_names.get(node.func.id)
        if arg_names is None:
            raise NotImplementedError(f"keyword arguments unsupported for {node.func.id}")
        values_by_name = {}
        for name, value in zip(arg_names, args):
            values_by_name[name] = value
        for keyword in node.keywords:
            if keyword.arg is None:
                raise NotImplementedError("**kwargs are not supported")
            values_by_name[keyword.arg] = self.expr(keyword.value, local_types)
        defaults = self.function_arg_defaults.get(node.func.id, {})
        for name in arg_names:
            if name not in values_by_name and name in defaults:
                values_by_name[name] = self.expr(defaults[name], local_types)
        return [values_by_name[name] for name in arg_names]

    def list_comp_expr(self, node: ast.ListComp, local_types: dict[str, str]) -> str:
        if len(node.generators) != 1:
            raise NotImplementedError("only single-generator list comprehensions are supported")
        gen = node.generators[0]
        if gen.ifs:
            raise NotImplementedError("list comprehension filters are not supported")
        if not isinstance(gen.target, ast.Name):
            raise NotImplementedError("only simple list comprehension targets are supported")
        loop_var = gen.target.id
        loop_var_type = local_types.get(loop_var, "double")
        result_type = "std::vector<double>"
        if (
            isinstance(gen.iter, ast.Call)
            and isinstance(gen.iter.func, ast.Name)
            and gen.iter.func.id == "range"
            and len(gen.iter.args) == 1
        ):
            stop_expr = self.expr(gen.iter.args[0], local_types)
            return (
                "[&]() { "
                + result_type
                + " result; "
                + "result.reserve("
                + stop_expr
                + "); "
                + "for (int "
                + loop_var
                + " = 0; "
                + loop_var
                + " < "
                + stop_expr
                + "; ++"
                + loop_var
                + ") { result.push_back("
                + self.expr(node.elt, local_types | {loop_var: "int"})
                + "); } return result; }()"
            )
        iter_expr = self.expr(gen.iter, local_types)
        return (
            "[&]() { "
            + result_type
            + " result; "
            + "for (const "
            + loop_var_type
            + "& "
            + loop_var
            + " : "
            + iter_expr
            + ") { result.push_back("
            + self.expr(node.elt, local_types | {loop_var: loop_var_type})
            + "); } return result; }()"
        )

    def compare_expr(self, node: ast.Compare, local_types: dict[str, str]) -> str:
        left_expr = self.expr(node.left, local_types)
        parts = []
        current_left_expr = left_expr

        for index, (op, right_node) in enumerate(zip(node.ops, node.comparators)):
            if isinstance(op, (ast.In, ast.NotIn)) and isinstance(right_node, ast.Set):
                tests = [f"({current_left_expr} == {self.expr(elt, local_types)})" for elt in right_node.elts]
                joined = " || ".join(tests) if tests else "false"
                if isinstance(op, ast.NotIn):
                    parts.append(f"!({joined})")
                else:
                    parts.append(f"({joined})")
            else:
                right_expr = self.expr(right_node, local_types)
                op_text = {
                    ast.Eq: "==",
                    ast.NotEq: "!=",
                    ast.Lt: "<",
                    ast.LtE: "<=",
                    ast.Gt: ">",
                    ast.GtE: ">=",
                }.get(type(op))
                if op_text is None:
                    raise NotImplementedError(f"unsupported comparison: {ast.dump(node)}")
                parts.append(f"({current_left_expr} {op_text} {right_expr})")
            if index < len(node.ops) - 1:
                current_left_expr = self.expr(right_node, local_types)

        if len(parts) == 1:
            return parts[0]
        return "(" + " && ".join(parts) + ")"

    def translate_string_method_call(self, node: ast.Call, local_types: dict[str, str]) -> str:
        chain = []
        current = node
        while isinstance(current, ast.Call) and isinstance(current.func, ast.Attribute):
            chain.append(current.func.attr)
            current = current.func.value
        base = self.expr(current, local_types)
        result = base
        for attr in reversed(chain):
            if attr == "strip":
                result = f"trim_copy({result})"
            elif attr == "lower":
                result = f"lower_copy({result})"
            else:
                raise NotImplementedError(f"unsupported string method: {attr}")
        return result

    def is_string_method_call(self, node: ast.Call) -> bool:
        current = node
        seen = False
        while isinstance(current, ast.Call) and isinstance(current.func, ast.Attribute):
            if current.func.attr not in {"strip", "lower"}:
                return False
            current = current.func.value
            seen = True
        return seen

    def attr_chain(self, node: ast.Attribute) -> list[str]:
        parts = [node.attr]
        value = node.value
        while isinstance(value, ast.Attribute):
            parts.append(value.attr)
            value = value.value
        if isinstance(value, ast.Name):
            parts.append(value.id)
        return list(reversed(parts))

    def find_main_call(self, tree: ast.Module) -> str | None:
        for node in tree.body:
            if not isinstance(node, ast.If):
                continue
            if (
                isinstance(node.test, ast.Compare)
                and isinstance(node.test.left, ast.Name)
                and node.test.left.id == "__name__"
                and len(node.test.ops) == 1
                and isinstance(node.test.ops[0], ast.Eq)
                and len(node.test.comparators) == 1
                and isinstance(node.test.comparators[0], ast.Constant)
                and node.test.comparators[0].value == "__main__"
            ):
                if len(node.body) == 1 and isinstance(node.body[0], ast.Expr):
                    call = node.body[0].value
                    if isinstance(call, ast.Call) and isinstance(call.func, ast.Name):
                        return call.func.id
        return None

    def joined_str_literal(self, node: ast.JoinedStr | ast.AST) -> str:
        if isinstance(node, ast.JoinedStr):
            parts = []
            for value in node.values:
                if not isinstance(value, ast.Constant) or not isinstance(value.value, str):
                    raise NotImplementedError("dynamic format specs are not supported")
                parts.append(value.value)
            return "".join(parts)
        if isinstance(node, ast.Constant) and isinstance(node.value, str):
            return node.value
        raise NotImplementedError("unsupported format spec")

    def string_literal_value(self, node: ast.AST) -> str:
        if isinstance(node, ast.Constant) and isinstance(node.value, str):
            return node.value
        raise NotImplementedError("only literal exception messages are supported")

    def is_docstring_stmt(self, stmt: ast.stmt) -> bool:
        return (
            isinstance(stmt, ast.Expr)
            and isinstance(stmt.value, ast.Constant)
            and isinstance(stmt.value.value, str)
        )

    def escape_cpp_string(self, value: str) -> str:
        return value.replace("\\", "\\\\").replace('"', '\\"').replace("\n", "\\n")

    def is_negative_expr(self, node: ast.AST) -> bool:
        if isinstance(node, ast.UnaryOp) and isinstance(node.op, ast.USub):
            return True
        if isinstance(node, ast.Constant) and isinstance(node.value, (int, float)):
            return node.value < 0
        return False

    def default_initializer(self, cpp_type: str) -> str:
        if cpp_type == "double":
            return " = 0.0"
        if cpp_type == "int":
            return " = 0"
        if cpp_type == "bool":
            return " = false"
        if cpp_type == "std::string":
            return ' = ""'
        return ""

    def tuple_inner_types(self, cpp_type: str) -> list[str] | None:
        if not cpp_type.startswith("std::tuple<") or not cpp_type.endswith(">"):
            return None
        inner = cpp_type[len("std::tuple<"):-1]
        parts: list[str] = []
        current = []
        depth = 0
        for ch in inner:
            if ch == "<":
                depth += 1
            elif ch == ">":
                depth -= 1
            if ch == "," and depth == 0:
                parts.append("".join(current).strip())
                current = []
                continue
            current.append(ch)
        if current:
            parts.append("".join(current).strip())
        return parts

    def squeeze_axis_expr(self, node: ast.AST, local_types: dict[str, str]) -> str:
        if isinstance(node, ast.Call) and isinstance(node.func, ast.Attribute):
            if self.attr_chain(node.func)[:2] == ["np", "squeeze"]:
                axis_kw = next((kw for kw in node.keywords if kw.arg == "axis"), None)
                if axis_kw is not None:
                    return self.expr(axis_kw.value, local_types)
        return "0"

    def current_function_name(self, local_types: dict[str, str]) -> str:
        for name, value in self.function_local_types.items():
            if value is local_types:
                return name
        return ""

    def infer_arg_type_from_usage(self, node: ast.FunctionDef, arg_name: str, local_types: dict[str, str]) -> str:
        inferred = "double"
        for inner in ast.walk(node):
            if isinstance(inner, ast.Call) and isinstance(inner.func, ast.Attribute):
                chain = self.attr_chain(inner.func)
                if chain[:2] in (["np", "max"], ["np", "sum"]) and inner.args:
                    if isinstance(inner.args[0], ast.Name) and inner.args[0].id == arg_name:
                        if any(keyword.arg == "axis" for keyword in inner.keywords):
                            return "pycpp::Array2D<double>"
                if inner.func.attr in {"ravel", "copy"} and isinstance(inner.func.value, ast.Name) and inner.func.value.id == arg_name:
                    inferred = "pycpp::Array1D<double>"
            if isinstance(inner, ast.Attribute) and inner.attr == "size":
                if isinstance(inner.value, ast.Name) and inner.value.id == arg_name:
                    inferred = "pycpp::Array1D<double>"
            if isinstance(inner, ast.Subscript) and isinstance(inner.value, ast.Name) and inner.value.id == arg_name:
                if isinstance(inner.slice, ast.Tuple):
                    return "pycpp::Array2D<double>"
                inferred = "pycpp::Array1D<double>"
        return inferred

    def binop(self, op: ast.operator) -> str:
        mapping = {
            ast.Add: "+",
            ast.Sub: "-",
            ast.Mult: "*",
            ast.Div: "/",
        }
        for kind, symbol in mapping.items():
            if isinstance(op, kind):
                return symbol
        raise NotImplementedError(f"unsupported binary operator: {type(op).__name__}")


def transpile_python_to_cpp(input_path: Path, output_path: Path) -> None:
    source = input_path.read_text(encoding="utf-8")
    tree = ast.parse(source)
    transpiler = CppTranspiler()
    cpp_source = transpiler.transpile(tree)
    output_path.write_text(cpp_source, encoding="utf-8")


def compile_command(source_path: Path, compiler: str, helper_paths: list[str]) -> list[str]:
    executable_path = source_path.with_suffix(".exe")
    return [
        compiler,
        "-std=c++17",
        "-O3",
        "-march=native",
        "-flto",
        "-o",
        str(executable_path),
        str(source_path),
        *helper_paths,
    ]


def compile_cpp(source_path: Path, compiler: str, helper_paths: list[str]) -> Path:
    executable_path = source_path.with_suffix(".exe")
    cmd = compile_command(source_path, compiler, helper_paths)
    cp = subprocess.run(cmd, capture_output=True, text=True)
    if cp.returncode != 0:
        raise RuntimeError(cp.stdout + cp.stderr)
    return executable_path


def run_executable(executable_path: Path, tee: bool) -> int:
    if tee:
        proc = subprocess.Popen([str(executable_path)], stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True)
        assert proc.stdout is not None
        for line in proc.stdout:
            print(line, end="")
        return proc.wait()
    cp = subprocess.run([str(executable_path)], capture_output=True, text=True)
    if cp.stdout:
        print(cp.stdout, end="")
    if cp.stderr:
        print(cp.stderr, end="", file=sys.stderr)
    return cp.returncode


def run_python(input_path: Path, tee: bool) -> int:
    cmd = [sys.executable, str(input_path)]
    if tee:
        proc = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True)
        assert proc.stdout is not None
        for line in proc.stdout:
            print(line, end="")
        return proc.wait()
    cp = subprocess.run(cmd, capture_output=True, text=True)
    if cp.stdout:
        print(cp.stdout, end="")
    if cp.stderr:
        print(cp.stderr, end="", file=sys.stderr)
    return cp.returncode


def timed_run_python(input_path: Path, tee: bool) -> tuple[int, float]:
    start = time.perf_counter()
    rc = run_python(input_path, tee)
    elapsed = time.perf_counter() - start
    return rc, elapsed


def timed_run_executable(executable_path: Path, tee: bool) -> tuple[int, float]:
    start = time.perf_counter()
    rc = run_executable(executable_path, tee)
    elapsed = time.perf_counter() - start
    return rc, elapsed


def capture_command_output(cmd: list[str]) -> tuple[int, str, str]:
    cp = subprocess.run(cmd, capture_output=True, text=True)
    return cp.returncode, cp.stdout or "", cp.stderr or ""


def compare_python_and_cpp(input_path: Path, executable_path: Path) -> int:
    python_rc, python_stdout, python_stderr = capture_command_output([sys.executable, str(input_path)])
    cpp_rc, cpp_stdout, cpp_stderr = capture_command_output([str(executable_path)])

    print("== Python stdout ==")
    if python_stdout:
        print(python_stdout, end="" if python_stdout.endswith("\n") else "\n")
    print("== C++ stdout ==")
    if cpp_stdout:
        print(cpp_stdout, end="" if cpp_stdout.endswith("\n") else "\n")

    if python_stderr:
        print("== Python stderr ==", file=sys.stderr)
        print(python_stderr, end="" if python_stderr.endswith("\n") else "\n", file=sys.stderr)
    if cpp_stderr:
        print("== C++ stderr ==", file=sys.stderr)
        print(cpp_stderr, end="" if cpp_stderr.endswith("\n") else "\n", file=sys.stderr)

    if python_rc != 0:
        print(f"Python run failed with exit code {python_rc}", file=sys.stderr)
        return python_rc
    if cpp_rc != 0:
        print(f"C++ run failed with exit code {cpp_rc}", file=sys.stderr)
        return cpp_rc

    if python_stdout == cpp_stdout:
        print("Outputs match")
        return 0

    print("Outputs differ")
    diff_lines = difflib.unified_diff(
        python_stdout.splitlines(),
        cpp_stdout.splitlines(),
        fromfile="python",
        tofile="cpp",
        lineterm="",
    )
    for line in diff_lines:
        print(line)
    return 1


def first_mismatch_line(left: str, right: str) -> int | None:
    left_lines = left.splitlines()
    right_lines = right.splitlines()
    for index, (left_line, right_line) in enumerate(zip(left_lines, right_lines), start=1):
        if left_line != right_line:
            return index
    if len(left_lines) != len(right_lines):
        return min(len(left_lines), len(right_lines)) + 1
    return None


def print_unified_diff(left: str, right: str, left_name: str, right_name: str) -> None:
    diff_lines = difflib.unified_diff(
        left.splitlines(),
        right.splitlines(),
        fromfile=left_name,
        tofile=right_name,
        lineterm="",
    )
    for line in diff_lines:
        print(line)


def print_timing_summary(
    python_elapsed: float,
    transpile_elapsed: float,
    compile_elapsed: float,
    cpp_elapsed: float,
) -> None:
    total_elapsed = transpile_elapsed + compile_elapsed + cpp_elapsed
    baseline = python_elapsed if python_elapsed > 0.0 else 1.0
    print("Timing summary (seconds):")
    print("  stage         seconds    ratio(vs python run)")
    print(f"  python run   {python_elapsed:9.6f}               {python_elapsed / baseline:9.6f}")
    print(f"  transpile    {transpile_elapsed:9.6f}               {transpile_elapsed / baseline:9.6f}")
    print(f"  compile      {compile_elapsed:9.6f}               {compile_elapsed / baseline:9.6f}")
    print(f"  c++ run      {cpp_elapsed:9.6f}               {cpp_elapsed / baseline:9.6f}")
    print(f"  total        {total_elapsed:9.6f}               {total_elapsed / baseline:9.6f}")


def main() -> int:
    parser = argparse.ArgumentParser(description="partial python -> c++ transpiler")
    parser.add_argument("input_py", help="input python source")
    parser.add_argument("helpers", nargs="*", help="zero or more helper .cpp files")
    parser.add_argument("--out", help="output .cpp path (default: input basename with _p.cpp)")
    parser.add_argument("--flat", action="store_true", help="accepted for xp2f.py compatibility")
    parser.add_argument("--compile", action="store_true", help="compile transpiled source")
    parser.add_argument("--run", action="store_true", help="compile and run transpiled source")
    parser.add_argument("--run-diff", action="store_true", help="run Python and transpiled C++ and compare outputs")
    parser.add_argument(
        "--time-both",
        action="store_true",
        help="run original Python and transpiled C++ and report timings for both",
    )
    parser.add_argument("--tee", action="store_true", help="stream output while running transpiled C++")
    parser.add_argument("--compiler", default="g++", help="C++ compiler executable (default: g++)")
    args = parser.parse_args()

    input_path = Path(args.input_py)
    output_path = Path(args.out) if args.out else input_path.with_name(f"{input_path.stem}_p.cpp")

    transpile_elapsed = 0.0
    try:
        transpile_start = time.perf_counter()
        transpile_python_to_cpp(input_path, output_path)
        transpile_elapsed = time.perf_counter() - transpile_start
    except Exception as exc:
        print(f"Transpile failed: {exc}", file=sys.stderr)
        return 1

    if not args.time_both:
        print(f"wrote {output_path}")

    if args.time_both:
        args.run = True
    if args.run_diff:
        args.run = True

    if not args.compile and not args.run:
        return 0

    compile_elapsed = 0.0
    try:
        compile_start = time.perf_counter()
        executable_path = compile_cpp(output_path, args.compiler, args.helpers)
        compile_elapsed = time.perf_counter() - compile_start
    except Exception as exc:
        print(f"Compile failed: {exc}", file=sys.stderr)
        return 1

    if not args.time_both:
        print(executable_path)

    if args.time_both:
        python_cmd = [sys.executable, str(input_path)]
        print(f"Run (python): {' '.join(python_cmd)}")
        python_start = time.perf_counter()
        python_rc, python_stdout, python_stderr = capture_command_output(python_cmd)
        python_elapsed = time.perf_counter() - python_start
        print(f"Run (python): {'PASS' if python_rc == 0 else 'FAIL'}")
        if python_stdout:
            print(python_stdout, end="" if python_stdout.endswith("\n") else "\n")
        if python_stderr:
            print(python_stderr, end="" if python_stderr.endswith("\n") else "\n", file=sys.stderr)
        if python_rc != 0:
            return python_rc

        print(f"wrote {output_path}")
        build_cmd = compile_command(output_path, args.compiler, args.helpers)
        compile_opts = " ".join(build_cmd[1:-3]) if not args.helpers else " ".join(build_cmd[1:6])
        print(f"Compile options: {compile_opts}")
        print(f"Build: {' '.join(build_cmd)}")
        print("Build: PASS")
        cpp_start = time.perf_counter()
        cpp_rc, cpp_stdout, cpp_stderr = capture_command_output([str(executable_path)])
        cpp_elapsed = time.perf_counter() - cpp_start
        print(f"Run: {'PASS' if cpp_rc == 0 else 'FAIL'}")
        if cpp_stdout:
            print(cpp_stdout, end="" if cpp_stdout.endswith("\n") else "\n")
        if cpp_stderr:
            print(cpp_stderr, end="" if cpp_stderr.endswith("\n") else "\n", file=sys.stderr)
        if python_stdout == cpp_stdout:
            print("Run diff: PASS")
        else:
            print("Run diff: DIFF")
            mismatch_line = first_mismatch_line(python_stdout, cpp_stdout)
            if mismatch_line is not None:
                python_lines = python_stdout.splitlines()
                cpp_lines = cpp_stdout.splitlines()
                python_line = python_lines[mismatch_line - 1] if mismatch_line <= len(python_lines) else ""
                cpp_line = cpp_lines[mismatch_line - 1] if mismatch_line <= len(cpp_lines) else ""
                print(f"  first mismatch line: {mismatch_line}")
                print(f"  python : {python_line}")
                print(f"  c++    : {cpp_line}")
            print_unified_diff(python_stdout, cpp_stdout, "python", "cpp")
        print()
        print_timing_summary(python_elapsed, transpile_elapsed, compile_elapsed, cpp_elapsed)
        return cpp_rc

    if args.run_diff:
        return compare_python_and_cpp(input_path, executable_path)

    if args.run:
        return run_executable(executable_path, args.tee)

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
