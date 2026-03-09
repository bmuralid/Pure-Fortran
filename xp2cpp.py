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

    def emit(self, line: str = "") -> None:
        self.lines.append("    " * self.indent_level + line)

    def transpile(self, tree: ast.Module) -> str:
        self.collect_function_types(tree)
        self.emit("#include <algorithm>")
        self.emit("#include <cctype>")
        self.emit("#include <cmath>")
        self.emit("#include <iomanip>")
        self.emit("#include <iostream>")
        self.emit("#include <sstream>")
        self.emit("#include <stdexcept>")
        self.emit("#include <string>")
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

        for node in tree.body:
            if isinstance(node, ast.FunctionDef):
                self.emit_function(node)
                self.emit()

        main_call = self.find_main_call(tree)
        self.emit("int main() {")
        self.indent_level += 1
        if main_call is not None:
            self.emit(f"{main_call}();")
        self.emit("return 0;")
        self.indent_level -= 1
        self.emit("}")
        return "\n".join(self.lines) + "\n"

    def collect_function_types(self, tree: ast.Module) -> None:
        for node in tree.body:
            if not isinstance(node, ast.FunctionDef):
                continue
            self.function_return_types[node.name] = self.cpp_type_from_annotation(node.returns)
            local_types = {}
            for arg in node.args.args:
                local_types[arg.arg] = self.cpp_type_from_annotation(arg.annotation)
            for stmt in node.body:
                self.collect_stmt_types(stmt, local_types)
            self.function_local_types[node.name] = local_types

    def collect_stmt_types(self, stmt: ast.stmt, local_types: dict[str, str]) -> None:
        if isinstance(stmt, ast.Assign) and len(stmt.targets) == 1 and isinstance(stmt.targets[0], ast.Name):
            local_types.setdefault(stmt.targets[0].id, self.infer_expr_type(stmt.value, local_types))
        elif isinstance(stmt, ast.If):
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
        if isinstance(expr, ast.Call):
            if isinstance(expr.func, ast.Name):
                if expr.func.id in {"abs", "float"}:
                    return "double"
                if expr.func.id == "int":
                    return "int"
                if expr.func.id in {"print"}:
                    return "void"
                return self.function_return_types.get(expr.func.id, "double")
            if isinstance(expr.func, ast.Attribute):
                if self.is_string_method_call(expr):
                    return "std::string"
                if self.attr_chain(expr.func)[:2] == ["math", "erf"]:
                    return "double"
        if isinstance(expr, ast.BinOp):
            return "double"
        if isinstance(expr, ast.UnaryOp):
            return self.infer_expr_type(expr.operand, local_types)
        if isinstance(expr, ast.Compare):
            return "bool"
        if isinstance(expr, ast.BoolOp):
            return "bool"
        return "double"

    def cpp_type_from_annotation(self, annotation: ast.AST | None) -> str:
        if annotation is None:
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
        return "double"

    def emit_function(self, node: ast.FunctionDef) -> None:
        return_type = self.function_return_types[node.name]
        args = []
        for arg in node.args.args:
            args.append(f"{self.cpp_type_from_annotation(arg.annotation)} {arg.arg}")
        self.emit(f"{return_type} {node.name}({', '.join(args)}) {{")
        self.indent_level += 1
        local_types = self.function_local_types[node.name]
        declared = set(arg.arg for arg in node.args.args)
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
        if isinstance(stmt, ast.Return):
            if stmt.value is None:
                self.emit("return;")
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
            self.emit(f"{self.expr(stmt.value, local_types)};")
            return
        raise NotImplementedError(f"unsupported statement: {ast.dump(stmt)}")

    def emit_assign(self, stmt: ast.Assign, local_types: dict[str, str], declared: set[str]) -> None:
        target = stmt.targets[0]
        if not isinstance(target, ast.Name):
            raise NotImplementedError("only simple assignments are supported")
        name = target.id
        value = self.expr(stmt.value, local_types)
        if name in declared:
            self.emit(f"{name} = {value};")
        else:
            cpp_type = local_types.get(name, self.infer_expr_type(stmt.value, local_types))
            self.emit(f"{cpp_type} {name} = {value};")
            declared.add(name)

    def emit_print(self, call: ast.Call, local_types: dict[str, str]) -> None:
        if len(call.args) != 1:
            raise NotImplementedError("print with multiple arguments is not supported")
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
            if isinstance(node.op, ast.Pow):
                return f"std::pow({self.expr(node.left, local_types)}, {self.expr(node.right, local_types)})"
            op = self.binop(node.op)
            return f"({self.expr(node.left, local_types)} {op} {self.expr(node.right, local_types)})"
        if isinstance(node, ast.UnaryOp):
            op = "+" if isinstance(node.op, ast.UAdd) else "-"
            return f"({op}{self.expr(node.operand, local_types)})"
        if isinstance(node, ast.Compare):
            return self.compare_expr(node, local_types)
        if isinstance(node, ast.BoolOp):
            op = "&&" if isinstance(node.op, ast.And) else "||"
            return "(" + f" {op} ".join(self.expr(v, local_types) for v in node.values) + ")"
        if isinstance(node, ast.Call):
            return self.call_expr(node, local_types)
        if isinstance(node, ast.Set):
            raise NotImplementedError("set literals are only supported in membership tests")
        if isinstance(node, ast.Attribute):
            chain = self.attr_chain(node)
            if chain == ["math", "pi"]:
                return "M_PI"
        raise NotImplementedError(f"unsupported expression: {ast.dump(node)}")

    def call_expr(self, node: ast.Call, local_types: dict[str, str]) -> str:
        if self.is_string_method_call(node):
            return self.translate_string_method_call(node, local_types)

        if isinstance(node.func, ast.Name):
            name = node.func.id
            if name == "abs":
                return f"std::abs({self.expr(node.args[0], local_types)})"
            if name in {"float", "int"}:
                target = "double" if name == "float" else "int"
                return f"static_cast<{target}>({self.expr(node.args[0], local_types)})"
            if name == "max":
                return f"std::max({self.expr(node.args[0], local_types)}, {self.expr(node.args[1], local_types)})"
            if name == "min":
                return f"std::min({self.expr(node.args[0], local_types)}, {self.expr(node.args[1], local_types)})"
            args = ", ".join(self.expr(arg, local_types) for arg in node.args)
            return f"{name}({args})"

        if isinstance(node.func, ast.Attribute):
            chain = self.attr_chain(node.func)
            if chain[:1] == ["math"]:
                cpp_name = {
                    "erf": "std::erf",
                    "sqrt": "std::sqrt",
                    "log": "std::log",
                    "exp": "std::exp",
                }.get(chain[1])
                if cpp_name is None:
                    raise NotImplementedError(f"unsupported math function: {'.'.join(chain)}")
                args = ", ".join(self.expr(arg, local_types) for arg in node.args)
                return f"{cpp_name}({args})"

        raise NotImplementedError(f"unsupported call: {ast.dump(node)}")

    def compare_expr(self, node: ast.Compare, local_types: dict[str, str]) -> str:
        if len(node.ops) != 1 or len(node.comparators) != 1:
            raise NotImplementedError("chained comparisons are not supported")
        left = self.expr(node.left, local_types)
        right_node = node.comparators[0]
        op = node.ops[0]

        if isinstance(op, (ast.In, ast.NotIn)) and isinstance(right_node, ast.Set):
            tests = [f"({left} == {self.expr(elt, local_types)})" for elt in right_node.elts]
            joined = " || ".join(tests) if tests else "false"
            if isinstance(op, ast.NotIn):
                return f"!({joined})"
            return f"({joined})"

        right = self.expr(right_node, local_types)
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
        return f"({left} {op_text} {right})"

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


def compile_cpp(source_path: Path, compiler: str, helper_paths: list[str]) -> Path:
    executable_path = source_path.with_suffix(".exe")
    cmd = [compiler, "-std=c++17", "-O2", "-o", str(executable_path), str(source_path), *helper_paths]
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

    try:
        transpile_python_to_cpp(input_path, output_path)
    except Exception as exc:
        print(f"Transpile failed: {exc}", file=sys.stderr)
        return 1

    print(output_path)

    if args.time_both:
        args.run = True
    if args.run_diff:
        args.run = True

    if not args.compile and not args.run:
        return 0

    try:
        executable_path = compile_cpp(output_path, args.compiler, args.helpers)
    except Exception as exc:
        print(f"Compile failed: {exc}", file=sys.stderr)
        return 1

    print(executable_path)

    if args.time_both:
        print("== Python ==")
        python_rc, python_elapsed = timed_run_python(input_path, args.tee)
        print(f"[time] python: {python_elapsed:.6f} s")
        if python_rc != 0:
            return python_rc

        print("== C++ ==")
        cpp_rc, cpp_elapsed = timed_run_executable(executable_path, args.tee)
        print(f"[time] c++:    {cpp_elapsed:.6f} s")
        return cpp_rc

    if args.run_diff:
        return compare_python_and_cpp(input_path, executable_path)

    if args.run:
        return run_executable(executable_path, args.tee)

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
