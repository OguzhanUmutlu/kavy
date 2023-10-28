const fs = require("fs");
const {argv, exit} = require("process");
const child_process = require("child_process");

const Color = {
    reset: (text: string) => `\x1B[0m${text}\x1B[0m`,
    red: (text: string) => `\x1B[31m${text}\x1B[39m`,
    blue: (text: string) => `\x1B[34m${text}\x1B[39m`,
};

enum TokenType {
    _, // So that tokens start with the ID of 1
    INT,
    FLOAT,
    STRING,
    SET,
    NOT,
    GREATER,
    SMALLER,
    GREATER_EQUAL,
    SMALLER_EQUAL,
    OR,
    AND,
    XOR,
    PLUS,
    MINUS,
    MULTIPLY,
    DIVIDE,
    MODULO,
    PARENTHESES_OPEN,
    PARENTHESES_CLOSE,
    SQUARE_BRACKET_OPEN,
    SQUARE_BRACKET_CLOSE,
    CURLY_BRACKET_OPEN,
    CURLY_BRACKET_CLOSE,
    LINE_BREAK,
    SEMICOLON,
    COMMA,
    WORD,
    SET_ADD,
    SET_SUBTRACT,
    SET_MULTIPLY,
    SET_DIVIDE,
    SET_MODULO,
    EQUALS,
    NOT_EQUALS,
    COLON,
}

enum ExpressionTypes {
    DEFINE_INT = 100,
    DEFINE_ARRAY,
    DEFINE_FLOAT,
    GET_INDEX,
    SET_INDEX,
    CALL,
    IF,
    SWITCH,
    LOOP,
    WHILE,
    FOR,
    FUNCTION,
    PARENT,
    BREAK,
    CONTINUE,
    RETURN,
    STATEMENT,
}

type Token = {
    type: TokenType.INT,
    index: number,
    value: string,
    int: number
} | {
    type: Exclude<TokenType, TokenType.INT>,
    index: number,
    value: string,
    base?: number
};

type Expression = DefineIntExpression | DefineFloatExpression | CallExpression |
    DefineArrayExpression | GetIndexExpression | SetIndexExpression | ParentExpression;
type AnyExpression = ParentExpression | Expression | Token;
type DefineIntExpression = {
    type: ExpressionTypes.DEFINE_INT,
    token: Token,
    name: Token,
    value: AnyExpression[],
    const: boolean,
    new: boolean,
    extra: any
};
type DefineFloatExpression = {
    type: ExpressionTypes.DEFINE_FLOAT,
    token: Token,
    name: Token,
    value: AnyExpression[],
    const: boolean,
    new: boolean,
    extra: any
};
type DefineArrayExpression = {
    type: ExpressionTypes.DEFINE_ARRAY,
    token: Token,
    name: Token,
    value: AnyExpression[][],
    const: boolean,
    new: boolean,
    extra: any
};
type GetIndexExpression = {
    type: ExpressionTypes.GET_INDEX,
    token: Token,
    name: Token,
    index: Token
};
type SetIndexExpression = {
    type: ExpressionTypes.SET_INDEX,
    token: Token,
    name: Token,
    index: Token,
    value: AnyExpression[]
};
type CallExpression = {
    type: ExpressionTypes.CALL,
    token: Token,
    name: Token,
    arguments: AnyExpression[][]
};
type ParentExpression = {
    type: ExpressionTypes.PARENT,
    token: Token,
    children: AnyExpression[]
};

type Statement =
    IfStatement
    | SwitchStatement
    | LoopStatement
    | WhileStatement
    | ForStatement
    | FunctionStatement
    | BreakStatement
    | ContinueStatement
    | ReturnStatement
    | ExpressionStatement
    | Expression;
type IfStatement = {
    type: ExpressionTypes.IF,
    token: Token,
    if: { requirement: AnyExpression[], scope: ScopeInfo },
    else: null | ScopeInfo
};
type SwitchStatement = {
    type: ExpressionTypes.SWITCH,
    token: Token,
    input: AnyExpression[],
    cases: { case: AnyExpression[], scope: ScopeInfo }[],
    default: null | ScopeInfo
};
type LoopStatement = {
    type: ExpressionTypes.LOOP,
    token: Token,
    scope: ScopeInfo
};
type WhileStatement = {
    type: ExpressionTypes.WHILE,
    token: Token,
    requirement: AnyExpression[],
    scope: ScopeInfo
};
type ForStatement = {
    type: ExpressionTypes.FOR,
    token: Token,
    init: AnyExpression[],
    requirement: AnyExpression[],
    step: AnyExpression[],
    scope: ScopeInfo
};
type FunctionStatement = {
    type: ExpressionTypes.FUNCTION,
    token: Token,
    name: Token,
    arguments: VariableHolder[],
    scope: ScopeInfo
};
type BreakStatement = {
    type: ExpressionTypes.BREAK,
    token: Token
};
type ContinueStatement = {
    type: ExpressionTypes.CONTINUE,
    token: Token
};
type ReturnStatement = {
    type: ExpressionTypes.RETURN,
    token: Token,
    value: AnyExpression[]
};
type ExpressionStatement = {
    type: ExpressionTypes.STATEMENT,
    token: Token,
    children: (Token | Expression)[]
};

enum VariableType {
    INT,
    FLOAT,
    STRING,
    ARRAY,
    FUNCTION,
}

type VariableHolder = { name: string, type: VariableType, const: boolean, id: number };
type FunctionHolder = { name: string, definition: FunctionStatement, id: number };

type ScopeInfo = {
    parent: ScopeInfo | null,
    groups: ParentExpression,
    variables: Record<string, VariableHolder>,
    functions: Record<string, FunctionHolder>,
    statements: Statement[]
};

type Macro = {
    name: string,
    value: string,
    tokens: Token[]
};

let runtimeId = 0;

const TokenLookup: Record<string, TokenType> = {
    "=": TokenType.SET,
    "!": TokenType.NOT,
    "~": TokenType.NOT,
    ">": TokenType.GREATER,
    "<": TokenType.SMALLER,
    "|": TokenType.OR,
    "&": TokenType.AND,
    "^": TokenType.XOR,
    "+": TokenType.PLUS,
    "-": TokenType.MINUS,
    "*": TokenType.MULTIPLY,
    "/": TokenType.DIVIDE,
    "%": TokenType.MODULO,
    "(": TokenType.PARENTHESES_OPEN,
    ")": TokenType.PARENTHESES_CLOSE,
    "[": TokenType.SQUARE_BRACKET_OPEN,
    "]": TokenType.SQUARE_BRACKET_CLOSE,
    "{": TokenType.CURLY_BRACKET_OPEN,
    "}": TokenType.CURLY_BRACKET_CLOSE,
    "\n": TokenType.LINE_BREAK,
    "\r": TokenType.LINE_BREAK,
    ";": TokenType.SEMICOLON,
    ",": TokenType.COMMA,
    ":": TokenType.COLON,
};

const EqualsTokenLookup: Record<number, TokenType> = {
    [TokenType.PLUS]: TokenType.SET_ADD,
    [TokenType.MINUS]: TokenType.SET_SUBTRACT,
    [TokenType.MULTIPLY]: TokenType.SET_MULTIPLY,
    [TokenType.DIVIDE]: TokenType.SET_DIVIDE,
    [TokenType.MODULO]: TokenType.SET_MODULO,
    [TokenType.SET]: TokenType.EQUALS,
    [TokenType.GREATER]: TokenType.GREATER_EQUAL,
    [TokenType.SMALLER]: TokenType.SMALLER_EQUAL,
    [TokenType.NOT]: TokenType.NOT_EQUALS,
};

// 0.08ms for TokenOtherLookup
const TokenOtherLookup: Record<string, TokenType> = {};
const __Place_a = "a".charCodeAt(0);
const __Place_z = "z".charCodeAt(0);
const __Place_A = "A".charCodeAt(0);
const __Place_Z = "Z".charCodeAt(0);
for (let i = 0; i < 10; i++) TokenOtherLookup[i] = TokenType.INT;
for (let i = __Place_a; i <= __Place_z; i++) TokenOtherLookup[String.fromCharCode(i)] = TokenType.WORD;
for (let i = __Place_A; i <= __Place_Z; i++) TokenOtherLookup[String.fromCharCode(i)] = TokenType.WORD;
TokenOtherLookup._ = TokenType.WORD;
TokenOtherLookup.$ = TokenType.WORD;
TokenOtherLookup["@"] = TokenType.WORD;
TokenOtherLookup["#"] = TokenType.WORD; // some extra word characters

const TokenOtherNextLookup: Record<number, number[ ]> = {
    [TokenType.WORD]: [TokenType.WORD, TokenType.INT], // myWord, myWord2 etc.
    [TokenType.INT]: [TokenType.INT] // only expects int after int: 12345
};

const BracketLookup = {
    [TokenType.PARENTHESES_OPEN]: TokenType.PARENTHESES_CLOSE,
    [TokenType.SQUARE_BRACKET_OPEN]: TokenType.SQUARE_BRACKET_CLOSE,
    [TokenType.CURLY_BRACKET_OPEN]: TokenType.CURLY_BRACKET_CLOSE,
};

/**
 * @param code
 * @param t
 * @param type
 * @param error
 * @throws Error
 */
function throwError(code: string, t: Token | AnyExpression, type: "SyntaxError" | "InternalError", error: string) {
    const token = "token" in t ? t.token : t;
    const lines = code.split("\n");
    let line = 0;
    let key = 0;
    const L = (token.value ?? " ").length;
    for (let i = 0; i <= token.index; i++) {
        key++;
        if (code[i] === "\n") {
            line++;
            key = 0;
        }
    }
    for (let i = -2; i <= 2; i++) {
        const l = line + i;
        if (!(l in lines)) continue;
        if (i === 0) {
            console.error(Color.red("> ") + Color.blue((l + 1) + " | " + lines[l].substring(0, key - 1)) + Color.red(lines[l].substring(key - 1, key - 1 + L) ?? "") + Color.blue(lines[l].substring(key - 1 + L)));
            console.error(" ".repeat(key + l.toString().length + 4) + Color.red("^".repeat(L)));
        } else {
            console.error(Color.blue("  " + (l + 1) + " | " + lines[l]));
        }
    }
    console.error("\n" + Color.red(type + ": " + error));
    exit(1);
}

const TokenizerIgnores = [
    " ", "\n", "\r", "\t"
];
const IntTokenTypes = {
    b: 2,
    o: 8,
    x: 16
};

const Whitespaces = [" ", "\n", "\r", "\t"];

function tokenize(code: string): Token[] {
    const tokens: Token[] = [];
    let openNot = false;
    const macros: Record<string, Macro> = {};
    for (let i = 0; i < code.length; i++) {
        const char = code[i];
        if (TokenizerIgnores.includes(char)) continue;
        if (char === "#") {
            const macro: Macro = {name: "", value: "", tokens: []};
            while (true) {
                i++;
                if (i === code.length) {
                    i--;
                    break;
                }
                const c2 = code[i];
                if (Whitespaces.includes(c2)) {
                    break;
                }
                macro.name += c2;
            }
            if (macros[macro.name]) throwError(code, {type: 0, index: i - macro.name.length - 1, value: "#" + macro.name}, "SyntaxError", "Cannot redeclare a macro.");
            let backslash = false;
            let svI = i;
            while (true) {
                i++;
                if (i === code.length) {
                    break;
                }
                const c2 = code[i];
                if (c2 === "\r") continue;
                if (c2 === "\n") {
                    if (!backslash) break;
                    macro.value = macro.value.slice(0, -1);
                    continue;
                }
                backslash = c2 === "\\";
                macro.value += c2;
            }
            macro.tokens = tokenize(macro.value);
            if (macro.tokens.length === 0) throwError(code, {type: 0, index: svI, value: " "}, "SyntaxError", "Expected a value for the macro.");
            macro.tokens.forEach(i => i.index += svI + 1);
            macros[macro.name] = macro;
            continue;
        }
        if (char === "/" && code[i + 1] === "/") {
            while (true) {
                i++;
                if (i === code.length) {
                    break;
                }
                const c2 = code[i];
                if (c2 === "\n") {
                    i--;
                    break;
                }
            }
            continue;
        }
        if (char === "/" && code[i + 1] === "*") {
            while (true) {
                i++;
                if (i === code.length) {
                    break;
                }
                const c2 = code[i - 1];
                const c3 = code[i];
                if (c2 === "*" && c3 === "/") {
                    break;
                }
            }
            continue;
        }
        const look = TokenLookup[char];
        if (look) {
            const last = tokens[tokens.length - 1];
            if (look === TokenType.SET) {
                const lookNew = EqualsTokenLookup[last?.type];
                if (lookNew) {
                    tokens.splice(tokens.length - 1, 1);
                    if (openNot) {
                        tokens.splice(tokens.length - 1, 1);
                    }
                    if (lookNew === TokenType.INT) {
                        throw new Error("Assumption failed.");
                    }
                    tokens.push({type: lookNew, value: last.value + char, index: last.index});
                    openNot = false;
                    continue;
                }
            }
            if (look === TokenType.NOT) {
                if (openNot) {
                    openNot = false;
                    tokens.splice(tokens.length - 2, 2);
                    continue;
                }
                openNot = true;
                tokens.push(
                    {type: TokenType.PARENTHESES_OPEN, index: i, value: "("}
                );
            } else if (openNot) {
                throwError(code, {
                    type: 0,
                    index: i,
                    value: " "
                }, "SyntaxError", "Expected a non-symbolic expression after not symbol.");
                return tokens;
            }
            if (last && last.type === look && (look === TokenType.PLUS || look === TokenType.MINUS)) {
                tokens.splice(tokens.length - 1, 1);
                tokens.push(
                    {
                        type: look === TokenType.PLUS ? TokenType.SET_ADD : TokenType.SET_SUBTRACT,
                        value: char + "=",
                        index: last.index
                    },
                    {
                        type: TokenType.INT,
                        value: "1",
                        index: last.index,
                        int: 1
                    }
                );
                continue;
            }
            if (look === TokenType.INT) {
                throw new Error("Assumption failed.");
            }
            tokens.push({type: look, value: char, index: i});
            continue;
        }
        if (char === "'") {
            if (code[i + 2] !== "'") throwError(code, {
                type: 0,
                index: i,
                value: "  "
            }, "SyntaxError", "Expected a quote(') after the character ")
            const val = code[i + 1];
            i += 2;
            tokens.push({type: TokenType.INT, index: i, value: char + val + char, int: val.charCodeAt(0)});
            continue;
        }
        if (char === '"') {
            let acc = "";
            const sI = i;
            let backslash = false;
            while (true) {
                i++;
                if (i === code.length) {
                    throwError(code, {type: 0, index: sI, value: " "}, "SyntaxError", "Expected the string to end.");
                    return tokens;
                }
                const c2 = code[i];
                if (c2 === "\n") {
                    throwError(code, {
                        type: 0,
                        index: i,
                        value: " "
                    }, "SyntaxError", "Expected (" + char + ") instead got a line break.");
                    return tokens;
                }
                if (c2 === char && !backslash) {
                    break;
                }
                if (c2 === "\\") backslash = !backslash;
                else backslash = false;
                acc += c2;
            }
            if (openNot) {
                throwError(code, {
                    type: 0,
                    index: sI,
                    value: " "
                }, "SyntaxError", "Cannot use the not symbol on a string literal.");
                return tokens;
            }
            tokens.push({type: TokenType.STRING, index: sI, value: char + acc + char});
            continue;
        }
        const otherLook = TokenOtherLookup[char];
        if (!otherLook) {
            throwError(code, {type: 0, index: i, value: " "}, "SyntaxError", "Undefined token '" + char + "'");
            return tokens;
        }
        const nextAssumption = TokenOtherNextLookup[otherLook];
        let acc = char;
        const sI = i;
        let intBase = 10;
        if (otherLook === TokenType.INT && char === "0") {
            const t2 = IntTokenTypes[code[i + 1]];
            if (t2) {
                i++;
                intBase = t2;
            }
        }
        while (true) {
            i++;
            if (i === code.length) {
                break;
            }
            const c2 = code[i];
            if (!nextAssumption.includes(TokenOtherLookup[c2])) {
                i--;
                break;
            }
            acc += c2;
        }
        if (otherLook === TokenType.INT) {
            tokens.push({type: TokenType.INT, index: sI, value: acc, int: parseInt(acc, intBase)});
        } else if (otherLook === TokenType.WORD) {
            const macro = macros[acc];
            if (macro) {
                if (macro.tokens.length > 1) tokens.push(
                    {type: TokenType.PARENTHESES_OPEN, index: sI, value: "("},
                    ...macro.tokens,
                    {type: TokenType.PARENTHESES_CLOSE, index: sI, value: ")"},
                ); else tokens.push(macro.tokens[0]);
            } else {
                tokens.push({type: otherLook, index: sI, value: acc});
            }
        } else throwError(code, {type: 0, index: sI, value: acc}, "SyntaxError", "Unexpected literal.");
        const last = tokens[tokens.length - 1];
        if (openNot) {
            tokens.push({type: TokenType.PARENTHESES_CLOSE, index: sI, value: ")"});
            openNot = false;
        }
        if (!last) continue;
        if (last.type === TokenType.MULTIPLY || last.type === TokenType.DIVIDE || last.type === TokenType.MODULO) {
            const last3 = tokens[tokens.length - 3];
            let nd = tokens.length - 3;
            if (last3.type === TokenType.PARENTHESES_CLOSE) {
                nd = tokens.length - 1;
            }
            tokens.splice(nd, 0, {
                type: TokenType.PARENTHESES_OPEN, index: tokens[nd].index, value: "("
            });
            tokens.push({type: TokenType.PARENTHESES_CLOSE, index: sI, value: ")"});
        }
    }
    if (openNot) {
        throwError(code, tokens[tokens.length - 1], "SyntaxError", "The not symbol wasn't completed.");
    }
    return tokens;
}

function group(code: string, tokens: Token[]): (ParentExpression & { deepness: number }) {
    const groups: any = {parent: null, end: null, token: null, children: []};
    let parent = groups;
    let deepness = 1;
    let maxDeepness = 1;
    if (tokens.length === 0) throw new Error("Unexpected empty parent expression.");
    for (let i = 0; i < tokens.length; i++) {
        const token = tokens[i];
        if (
            token.type === TokenType.PARENTHESES_OPEN ||
            token.type === TokenType.SQUARE_BRACKET_OPEN ||
            token.type === TokenType.CURLY_BRACKET_OPEN
        ) {
            parent = {parent, end: BracketLookup[token.type], token, children: []};
            deepness++;
            maxDeepness = Math.max(deepness, maxDeepness);
            continue;
        }
        if (parent.end === token.type) {
            const children = parent.children;
            const token = parent.token;
            parent = parent.parent;
            parent.children.push({type: ExpressionTypes.PARENT, token: token, children});
            deepness--;
            continue;
        }
        parent.children.push(token);
    }
    if (parent !== groups) {
        throwError(code, parent.token, "SyntaxError", "The bracket was never closed.");
    }
    return {
        type: ExpressionTypes.PARENT,
        token: tokens[0],
        children: groups.children,
        deepness: maxDeepness
    };
}

function nextUntilSemicolon(groups: ParentExpression, index: number, code: string, token: Token): [(Token | Expression)[], number] {
    const list: (Token | Expression)[] = [];
    for (let i = index; i < groups.children.length; i++) {
        const group = groups.children[i];
        if (group.type !== ExpressionTypes.PARENT && group.type === TokenType.SEMICOLON) return [list, i];
        list.push(group);
    }
    throwError(code, token, "SyntaxError", "Expected a semicolon after the expression.");
    return [list, groups.children.length];
}

function findVariable(info: ScopeInfo, name: string): { parent: ScopeInfo, value: VariableHolder } | null {
    if (name in info.variables) return {
        parent: info,
        value: info.variables[name]
    };
    if (info.parent) return findVariable(info.parent, name);
    return null;
}

function findFunction(info: ScopeInfo, name: string): { parent: ScopeInfo, value: FunctionHolder } | null {
    if (name in info.functions) return {
        parent: info,
        value: info.functions[name]
    };
    if (info.parent) return findFunction(info.parent, name);
    return null;
}

function splitTokensWithComma(group: AnyExpression[], code: string, split = TokenType.COMMA): AnyExpression[][] {
    if (group.length === 0) return [];
    const list: AnyExpression[][] = [[]];
    for (let i = 0; i < group.length; i++) {
        const current = group[i];
        if (current.type === split) {
            list.push([]);
            if (split === TokenType.COMMA && i === group.length - 1) {
                throwError(code, current, "SyntaxError", "Expected an expression after the comma.");
                return list;
            }
            continue;
        }
        list[list.length - 1].push(current);
    }
    return list;
}

const SetOperations: number[] = [
    TokenType.SET,
    TokenType.SET_ADD,
    TokenType.SET_SUBTRACT,
    TokenType.SET_MULTIPLY,
    TokenType.SET_DIVIDE,
    TokenType.SET_MODULO,
];

const SetOperatorLookup: Record<number, number> = {
    [TokenType.SET_ADD]: TokenType.PLUS,
    [TokenType.SET_SUBTRACT]: TokenType.MINUS,
    [TokenType.SET_MULTIPLY]: TokenType.MULTIPLY,
    [TokenType.SET_DIVIDE]: TokenType.DIVIDE,
    [TokenType.SET_MODULO]: TokenType.MODULO,
};

function ast(code: string, groups: ParentExpression, parent: ScopeInfo | null, allStatements: Statement[] = [], allVariables: VariableHolder[], useParentScopeVariables = false) {
    const statements: Statement[] = [];
    const scopeInfo: ScopeInfo = {
        parent: parent,
        groups,
        variables: useParentScopeVariables && parent ? parent.variables : {},
        functions: useParentScopeVariables && parent ? parent.functions : {},
        statements
    };
    const ASTWordLookup: Record<string, (group: Token) => void> = {
        "break"(group) {
            const statement: BreakStatement = {
                type: ExpressionTypes.BREAK, token: group
            };
            allStatements.push(statement);
            statements.push(statement);
        },
        "continue"(group) {
            const statement: ContinueStatement = {
                type: ExpressionTypes.CONTINUE, token: group
            };
            allStatements.push(statement);
            statements.push(statement);
        },
        "return"(group) {
            const [value, nI1] = nextUntilSemicolon(groups, i + 1, code, group);
            i = nI1;
            const statement: ReturnStatement = {
                type: ExpressionTypes.RETURN,
                token: group,
                value
            };
            allStatements.push(statement);
            statements.push(statement);
        },
        "let"(group) {
            const name = groups.children[++i];
            if (!name || name.type !== TokenType.WORD) {
                throwError(code, group, "SyntaxError", "Expected a valid variable name.");
                return;
            }
            const equals = groups.children[++i];
            if (!equals || equals.type !== TokenType.SET) {
                throwError(code, {
                    type: 0,
                    index: name.index + name.value.length,
                    value: " "
                }, "SyntaxError", "Expected '='");
                return;
            }
            const [value, nI3] = nextUntilSemicolon(groups, i + 1, code, group);
            i = nI3;
            const existing = scopeInfo.variables[name.value];
            if (existing) {
                throwError(code, group, "SyntaxError", "Cannot redeclare a variable: '" + name.value + "'.");
                return;
            }
            allVariables.push(scopeInfo.variables[name.value] = {
                type: VariableType.INT,
                name: name.value,
                const: group.value === "const",
                id: ++runtimeId
            });
            const statement: DefineIntExpression = {
                type: ExpressionTypes.DEFINE_INT,
                token: group,
                name: <Token>name,
                value,
                const: group.value === "const",
                new: true,
                extra: null
            };
            allStatements.push(statement);
            statements.push(statement);
        },
        "function"(group) {
            const name = groups.children[++i];
            if (!name || name.type === ExpressionTypes.PARENT || name.type !== TokenType.WORD) {
                throwError(code, group, "SyntaxError", "Expected a valid function name.");
                return;
            }
            const argumentList = groups.children[++i];
            if (!argumentList || argumentList.type !== ExpressionTypes.PARENT) {
                throwError(code, group, "SyntaxError", "Expected an argument list for the function statement.");
                return;
            }
            let scope = groups.children[++i];
            if (!scope) {
                throwError(code, group, "SyntaxError", "Expected a scope for the function statement.");
                return;
            }
            if (scope.type !== ExpressionTypes.PARENT) {
                const [value, nI1] = nextUntilSemicolon(groups, i, code, group);
                i = nI1;
                const index = "token" in value[0] ? value[0].token.index : value[0].index;
                value.push({type: TokenType.SEMICOLON, index, value: ";"});
                scope = {
                    type: ExpressionTypes.PARENT,
                    token: {type: 0, index, value: " "},
                    children: value
                };
            }
            const vars: VariableHolder[] = [];
            const varObj: Record<string, VariableHolder> = {};
            for (let i = 0; i < argumentList.children.length; i += 2) {
                const argName: AnyExpression = argumentList.children[i];
                const comma = argumentList.children[i + 1];
                if ("token" in argName) {
                    throwError(code, argName, "SyntaxError", "Expected argument of a function to be a valid variable name.");
                    return;
                }
                if (i !== argumentList.children.length - 1 && (!comma || comma.type !== TokenType.COMMA)) {
                    throwError(code, comma, "SyntaxError", "Expected a comma.");
                    return;
                }
                const variable = {
                    type: VariableType.INT,
                    name: argName.value,
                    const: false,
                    id: ++runtimeId
                };
                allVariables.push(variable);
                vars.push(variable);
                if (argName.value in varObj) {
                    throwError(code, argName, "SyntaxError", "Function's argument names should be unique.");
                }
                varObj[argName.value] = variable;
            }
            const statement: FunctionStatement = {
                type: ExpressionTypes.FUNCTION,
                token: group,
                name,
                arguments: vars,
                scope: ast(code, scope, {
                    parent: scopeInfo,
                    variables: varObj, functions: {}, statements: [], groups: {
                        type: ExpressionTypes.PARENT, token: group, children: []
                    }
                }, allStatements, allVariables, true)
            };
            if (scopeInfo.functions[name.value]) {
                throwError(code, group, "SyntaxError", "The function named '" + name.value + "' is already defined in the scope.");
            }
            scopeInfo.functions[name.value] = {
                name: name.value,
                id: ++runtimeId,
                definition: statement
            };
            allStatements.push(statement);
            statements.push(statement);
        },
        "if"(group) {
            const requirement = groups.children[++i];
            if (!requirement || requirement.type !== ExpressionTypes.PARENT) {
                throwError(code, group, "SyntaxError", "Expected a requirement for the if statement.");
                return;
            }
            if (requirement.children.length === 0) {
                throwError(code, group, "SyntaxError", "Expected the requirement of the if statement to not be empty.");
                return;
            }
            let scope = groups.children[++i];
            if (!scope) {
                throwError(code, group, "SyntaxError", "Expected a scope for the if statement.");
                return;
            }
            if (scope.type !== ExpressionTypes.PARENT) {
                const [value, nI1] = nextUntilSemicolon(groups, i, code, group);
                i = nI1;
                const index = "token" in value[0] ? value[0].token.index : value[0].index;
                value.push({type: TokenType.SEMICOLON, index, value: ";"});
                scope = {
                    type: ExpressionTypes.PARENT,
                    token: {type: 0, index, value: " "},
                    children: value
                };
            }
            const statement: IfStatement = {
                type: ExpressionTypes.IF,
                token: group,
                if: {
                    scope: ast(code, scope, scopeInfo, allStatements, allVariables, true),
                    requirement: requirement.children
                },
                else: null
            };
            allStatements.push(statement);
            statements.push(statement);
        },
        "else"(group) {
            const lastStatement = <IfStatement>statements[statements.length - 1];
            if (!lastStatement || lastStatement.type !== ExpressionTypes.IF) {
                throwError(code, group, "SyntaxError", "Unexpected else statement.");
                return;
            }
            const next = groups.children[++i];
            if (next && next.type === ExpressionTypes.PARENT) {
                // } else {
                if (lastStatement.else) {
                    throwError(code, group, "SyntaxError", "Else statement's bounding if statement already has an else statement.");
                    return;
                }
                lastStatement.else = ast(code, next, scopeInfo, allStatements, allVariables, true);
                return;
            }
            if (!next || next.type !== TokenType.WORD) {
                throwError(code, group, "SyntaxError", "Expected 'if'");
                return;
            }
            // } else if() {
            if (next.value !== "if") {
                throwError(code, group, "SyntaxError", "Expected the 'if' keyword.");
                return;
            }
            const requirement = groups.children[++i];
            if (!requirement || requirement.type !== ExpressionTypes.PARENT) {
                throwError(code, group, "SyntaxError", "Expected a requirement for the else-if statement.");
                return;
            }
            if (requirement.children.length === 0) {
                throwError(code, group, "SyntaxError", "Expected the requirement of the else-if statement to not be empty.");
                return;
            }
            let scope = groups.children[++i];
            if (!scope) {
                throwError(code, group, "SyntaxError", "Expected a scope for the else-if statement.");
                return;
            }
            if (scope.type !== ExpressionTypes.PARENT) {
                const [value, nI1] = nextUntilSemicolon(groups, i, code, group);
                i = nI1;
                const index = "token" in value[0] ? value[0].token.index : value[0].index;
                value.push({type: TokenType.SEMICOLON, index, value: ";"});
                scope = {
                    type: ExpressionTypes.PARENT,
                    token: {type: 0, index, value: " "},
                    children: value
                };
            }
            lastStatement.else = {
                statements: [{
                    type: ExpressionTypes.IF, token: next, else: null, if: {
                        scope: ast(code, scope, scopeInfo, allStatements, allVariables, true),
                        requirement: requirement.children
                    }
                }], groups: scope, parent: scopeInfo, variables: {}, functions: {}
            };
        },
        "elseif"(group) {
            const lastStatement = <IfStatement>statements[statements.length - 1];
            if (!lastStatement || lastStatement.type !== ExpressionTypes.IF) {
                throwError(code, group, "SyntaxError", "Unexpected else-if statement.");
                return;
            }
            const requirement = groups.children[++i];
            if (!requirement || requirement.type !== ExpressionTypes.PARENT) {
                throwError(code, group, "SyntaxError", "Expected a requirement for the else-if statement.");
                return;
            }
            if (requirement.children.length === 0) {
                throwError(code, group, "SyntaxError", "Expected the requirement of the else-if statement to not be empty.");
                return;
            }
            let scope = groups.children[++i];
            if (!scope) {
                throwError(code, group, "SyntaxError", "Expected a scope for the else-if statement.");
                return;
            }
            if (scope.type !== ExpressionTypes.PARENT) {
                const [value, nI1] = nextUntilSemicolon(groups, i, code, group);
                i = nI1;
                const index = "token" in value[0] ? value[0].token.index : value[0].index;
                value.push({type: TokenType.SEMICOLON, index, value: ";"});
                scope = {
                    type: ExpressionTypes.PARENT,
                    token: {type: 0, index, value: " "},
                    children: value
                };
            }
            lastStatement.else = {
                statements: [{
                    type: ExpressionTypes.IF, token: group, else: null, if: {
                        scope: ast(code, scope, scopeInfo, allStatements, allVariables, true),
                        requirement: requirement.children
                    }
                }], groups: scope, parent: scopeInfo, variables: {}, functions: {}
            };
        },
        "loop"(group) {
            let scope = groups.children[++i];
            if (!scope) {
                throwError(code, group, "SyntaxError", "Expected a scope for the loop statement.");
                return;
            }
            if (scope.type !== ExpressionTypes.PARENT) {
                const [value, nI1] = nextUntilSemicolon(groups, i, code, group);
                i = nI1;
                const index = "token" in value[0] ? value[0].token.index : value[0].index;
                value.push({type: TokenType.SEMICOLON, index, value: ";"});
                scope = {
                    type: ExpressionTypes.PARENT,
                    token: {type: 0, index, value: " "},
                    children: value
                };
            }
            const statement: LoopStatement = {
                type: ExpressionTypes.LOOP,
                token: group,
                scope: ast(code, scope, scopeInfo, allStatements, allVariables)
            };
            allStatements.push(statement);
            statements.push(statement);
        },
        "while"(group) {
            const requirement = groups.children[++i];
            if (!requirement || requirement.type !== ExpressionTypes.PARENT) {
                throwError(code, group, "SyntaxError", "Expected a requirement for the while statement.");
                return;
            }
            if (requirement.children.length === 0) {
                throwError(code, group, "SyntaxError", "Expected the requirement of the while statement to not be empty.");
                return;
            }
            let scope = groups.children[++i];
            if (!scope) {
                throwError(code, group, "SyntaxError", "Expected a scope for the while statement.");
                return;
            }
            if (scope.type !== ExpressionTypes.PARENT) {
                const [value, nI1] = nextUntilSemicolon(groups, i, code, group);
                i = nI1;
                const index = "token" in value[0] ? value[0].token.index : value[0].index;
                value.push({type: TokenType.SEMICOLON, index, value: ";"});
                scope = {
                    type: ExpressionTypes.PARENT,
                    token: {type: 0, index, value: " "},
                    children: value
                };
            }
            const scopeAst = ast(code, scope, scopeInfo, allStatements, allVariables);

            scopeAst.statements.splice(0, 0, {
                type: ExpressionTypes.IF, token: group, if: {
                    requirement: requirement.children, scope: {
                        statements: [
                            {
                                type: ExpressionTypes.BREAK, token: group
                            }
                        ], parent: scopeAst, variables: {}, functions: {}, groups: {
                            type: ExpressionTypes.PARENT, token: group, children: []
                        }
                    }
                }, else: null
            });
            const statement: LoopStatement = {
                type: ExpressionTypes.LOOP,
                token: group,
                scope: scopeAst
            };
            allStatements.push(statement);
            statements.push(statement);
        },
        "for"(group) {
            const requirement = groups.children[++i];
            if (!requirement || requirement.type !== ExpressionTypes.PARENT) {
                throwError(code, group, "SyntaxError", "Expected a requirement for the while statement.");
                return;
            }
            if (requirement.children.length === 0) {
                throwError(code, group, "SyntaxError", "Expected the requirement of the while statement to not be empty.");
                return;
            }
            let scope = groups.children[++i];
            if (!scope) {
                throwError(code, group, "SyntaxError", "Expected a scope for the while statement.");
                return;
            }
            if (scope.type !== ExpressionTypes.PARENT) {
                const [value, nI1] = nextUntilSemicolon(groups, i, code, group);
                i = nI1;
                const index = "token" in value[0] ? value[0].token.index : value[0].index;
                value.push({type: TokenType.SEMICOLON, index, value: ";"});
                scope = {
                    type: ExpressionTypes.PARENT,
                    token: {type: 0, index, value: " "},
                    children: value
                };
            }
            const split = splitTokensWithComma(requirement.children, code, TokenType.SEMICOLON);
            if (split.length !== 3) {
                throwError(code, group, "SyntaxError", "Expected one initial statement, one requirement and one step statement for the for-loop statement.");
            }
            const initialAst = ast(code, {
                type: ExpressionTypes.PARENT,
                token: group,
                children: [...split[0], {type: TokenType.SEMICOLON, value: ";", index: group.index}]
            }, scopeInfo, allStatements, allVariables, true);
            const stepAst = ast(code, {
                type: ExpressionTypes.PARENT,
                token: group,
                children: [...split[2], {type: TokenType.SEMICOLON, value: ";", index: group.index}]
            }, scopeInfo, allStatements, allVariables);
            const scopeAst = ast(code, scope, scopeInfo, allStatements, allVariables);
            allStatements.push(...initialAst.statements);
            statements.push(...initialAst.statements);
            scopeAst.statements.splice(0, 0, {
                type: ExpressionTypes.IF, token: group, if: {
                    requirement: [...split[1]], scope: {
                        statements: [], parent: scopeAst, variables: {}, functions: {}, groups: {
                            type: ExpressionTypes.PARENT, token: group, children: []
                        }
                    }
                }, else: {
                    statements: [
                        {
                            type: ExpressionTypes.BREAK, token: group
                        }
                    ], parent: scopeAst, variables: {}, functions: {}, groups: {
                        type: ExpressionTypes.PARENT, token: group, children: []
                    }
                }
            });
            scopeAst.statements.push(...stepAst.statements);
            const statement: LoopStatement = {
                type: ExpressionTypes.LOOP,
                token: group,
                scope: scopeAst
            };
            allStatements.push(statement);
            statements.push(statement);
        }
    };
    ASTWordLookup.const = ASTWordLookup.let;
    let i = 0;
    for (; i < groups.children.length; i++) {
        const group = groups.children[i];
        if (group.type === TokenType.LINE_BREAK) continue;
        if (group.type === TokenType.SEMICOLON) continue;
        if (group.type === TokenType.INT) continue;
        if (group.type === TokenType.FLOAT) continue;
        if (group.type === TokenType.STRING) continue;
        if (group.type === TokenType.WORD) {
            const look = ASTWordLookup[group.value];
            if (look) {
                look(group);
                continue;
            }
            const next = groups.children[++i];
            if (!next) {
                throwError(code, group, "SyntaxError", "Unexpected ending.");
                continue;
            }
            if (SetOperations.includes(next.type)) {
                const [value, nI2] = nextUntilSemicolon(groups, i + 1, code, group);
                i = nI2;
                const existing = findVariable(scopeInfo, group.value);
                if (!existing) {
                    throwError(code, group, "SyntaxError", "Undefined variable: '" + group.value + "'.");
                    return scopeInfo;
                }
                if (existing.value.const) {
                    throwError(code, group, "SyntaxError", "Cannot alter variables that are defined as constants.");
                    return scopeInfo;
                }
                if (next.type !== TokenType.SET) {
                    value.splice(
                        0, 0,
                        {type: TokenType.WORD, value: group.value, index: group.index},
                        {type: SetOperatorLookup[next.type], value: " ", index: group.index}
                    );
                }
                // TODO: check type
                const statement: DefineIntExpression = {
                    type: ExpressionTypes.DEFINE_INT,
                    token: group,
                    name: group,
                    const: false,
                    value: value,
                    new: false,
                    extra: null
                };
                allStatements.push(statement);
                statements.push(statement);
                continue;
            }
            const [line, nI1] = nextUntilSemicolon(groups, i, code, group);
            i = nI1;
            /*const statement: CallExpression = {
                type: ExpressionTypes.CALL,
                token: group,
                name: group,
                arguments: splitTokensWithComma(next.children, code)
            };
            allStatements.push(statement);
            statements.push(statement);*/
            const statement: ExpressionStatement = {
                type: ExpressionTypes.STATEMENT,
                token: group,
                children: [group, ...line]
            };
            allStatements.push(statement);
            statements.push(statement);
            continue;
            /*throwError(code, group,"SyntaxError", "Unexpected token: '" + group.value + "'");
            return scopeInfo;*/
        }
        throwError(code, group, "SyntaxError", "Unexpected token.");
        return scopeInfo;
    }
    return scopeInfo;
}

const expressionIgnores: number[] = [
    TokenType.COMMA,
    TokenType.LINE_BREAK
];
const operators: number[] = [
    TokenType.PLUS,
    TokenType.MINUS,
    TokenType.MULTIPLY,
    TokenType.DIVIDE,
    TokenType.MODULO,
    TokenType.EQUALS,
    TokenType.NOT_EQUALS,
    TokenType.GREATER_EQUAL,
    TokenType.SMALLER_EQUAL,
    TokenType.GREATER,
    TokenType.SMALLER,
    TokenType.AND,
    TokenType.OR,
    TokenType.XOR
];
const nonOperators: number[] = [
    TokenType.WORD,
    TokenType.INT,
    TokenType.FLOAT,
    ExpressionTypes.PARENT,
];

type RecursiveTokens = (Token | CallExpression | RecursiveTokens)[];

function filterExpressionAssembly(code: string, group: AnyExpression[]) {
    const filtered: RecursiveTokens = [];
    let ind = 0;
    for (let i = 0; i < group.length; i++) {
        const expression = group[i];
        if (expressionIgnores.includes(expression.type) || ("token" in expression && expression.type !== ExpressionTypes.PARENT)) continue;
        if (!nonOperators.includes(expression.type) && !operators.includes(expression.type)) {
            throwError(code, expression, "SyntaxError", "Unexpected token inside an expression.");
        }
        const isOperator = operators.includes(expression.type);
        if ((ind % 2 === 1) !== isOperator) {
            throwError(code, expression, "SyntaxError", "Expected an " + (isOperator ? "expression" : "operator") + ", got an " + (isOperator ? "operator" : "expression") + ".");
        }
        let ps: Token | CallExpression | RecursiveTokens;
        if (expression.type === ExpressionTypes.PARENT) {
            ps = filterExpressionAssembly(code, expression.children);
        } else {
            ps = expression;
            if (expression.type === TokenType.WORD) {
                const next = group[i + 1];
                if (next && next.type === ExpressionTypes.PARENT) {
                    i++;
                    ps = <CallExpression>{
                        type: ExpressionTypes.CALL,
                        token: expression,
                        name: expression,
                        arguments: splitTokensWithComma(next.children, code)
                    };
                }
            }
        }
        filtered.push(ps);
        ind++;
    }
    return filtered;
}

function assembleExpression(
    code: string,
    filtered: RecursiveTokens,
    strings: Record<string, number>,
    scope: ScopeInfo,
    currentDeepness = 0,
    lastSection: string[],
    sections: Record<string, string[]>
) {
    const first = filtered[0];

    const CallLookup: Record<string, Function> = {
        exit(statement: CallExpression) {
            const filtered = filterExpressionAssembly(code, statement.arguments[0]);
            lastSection = assembleExpression(code, filtered, strings, scope, currentDeepness, lastSection, sections);
            lastSection.push(
                `mov eax, 1`,
                `mov ebx, [_temp${currentDeepness}]`,
                `int 0x80`
            );
            lastSection.push(`mov ecx, 0`);
        },
        i_print(statement: CallExpression) {
            for (let i = 0; i < statement.arguments.length; i++) {
                const arg = statement.arguments[i];
                if (arg.length === 1 && arg[0].type === TokenType.STRING) {
                    lastSection.push(
                        `mov eax, 4`,
                        `mov ebx, 1`,
                        `mov ecx, _${strings[arg[0].value]}`,
                        `mov edx, ${arg[0].value.length - 2}`,
                        `int 0x80`
                    );
                } else {
                    const filtered = filterExpressionAssembly(code, arg);
                    lastSection = assembleExpression(code, filtered, strings, scope, currentDeepness, lastSection, sections);
                    lastSection.push(
                        `mov eax, 4`,
                        `mov ebx, 1`,
                        `mov ecx, _temp${currentDeepness}`,
                        `mov edx, 1`,
                        `int 0x80`
                    );
                }
            }
            lastSection.push(`mov ecx, 0`);
        },
        printf(statement: CallExpression) {
            const format = statement.arguments[0];
            if (!format) throwError(code, statement, "SyntaxError", "Expected a string as the first argument to the function printf().");

        }
    };

    function callFunction(expression: CallExpression) {
        const look = CallLookup[expression.name.value];
        if (look) return look(expression);
        const func = findFunction(scope, expression.name.value);
        if (!func) {
            throwError(code, expression.token, "SyntaxError", "Undefined function: '" + expression.name.value + "'");
            return;
        }
        const givenArgs = expression.arguments;
        const args = func.value.definition.arguments;
        if (givenArgs.length !== args.length) {
            throwError(code, expression.token, "SyntaxError", "Expected " + args.length + " arguments, instead got " + givenArgs.length + ".");
        }
        for (let i = 0; i < args.length; i++) {
            const arg = args[i];
            const givenArg = givenArgs[i];
            const filtered = filterExpressionAssembly(code, givenArg);
            lastSection = assembleExpression(code, filtered, strings, scope, currentDeepness, lastSection, sections);
            lastSection.push(
                `mov eax, [_temp${currentDeepness}]`,
                `mov [_var${arg.id}], eax`
            );
        }
        lastSection.push(`call _func${func.value.id}`);
    }

    if (Array.isArray(first)) {
        lastSection = assembleExpression(code, first, strings, scope, currentDeepness + 1, lastSection, sections);
        lastSection.push(
            `mov eax, [_temp${currentDeepness + 1}]`,
            `mov [_temp${currentDeepness}], eax`
        );
    } else if (first.type === TokenType.INT) {
        lastSection.push(
            `mov eax, ${first.int}`,
            `mov [_temp${currentDeepness}], eax`
        );
    } else if (first.type === TokenType.WORD) {
        // variable
        const variable = findVariable(scope, first.value);
        if (!variable) {
            throwError(code, first, "SyntaxError", "Variable not defined: '" + first.value + "'");
            return [];
        }
        lastSection.push(
            `mov eax, [_var${variable.value.id}]`,
            `mov [_temp${currentDeepness}], eax`
        );
    } else if (first.type === ExpressionTypes.CALL) {
        callFunction(first);
        lastSection.push(
            `mov [_temp${currentDeepness}], ecx`
        );
    } else {
        throwError(code, first, "SyntaxError", "Unexpected token.");
    }

    function normalOperatorLookup(expression: Token | CallExpression | RecursiveTokens) {
        let el: string;
        if (Array.isArray(expression)) {
            lastSection = assembleExpression(code, expression, strings, scope, currentDeepness + 1, lastSection, sections);
            el = `[_temp${currentDeepness + 1}]`;
        } else if (expression.type === ExpressionTypes.CALL) {
            callFunction(expression);
            el = `ecx`;
        } else {
            if (
                expression.type !== TokenType.INT &&
                expression.type !== TokenType.WORD
            ) {
                throwError(code, expression, "SyntaxError", "Expected an integer or a variable.");
            }
            if (expression.type === TokenType.INT) {
                el = expression.int.toString();
            } else if (expression.type === TokenType.WORD) {
                const variable = findVariable(scope, expression.value);
                if (!variable) {
                    throwError(code, expression, "SyntaxError", "Variable not defined: '" + expression.value + "'");
                    return "";
                }
                el = `[_var${variable.value.id}]`;
            } else {
                throwError(code, expression, "SyntaxError", "Unexpected token.");
                return "";
            }
        }
        return el;
    }

    function compareOperatorLookup(a: string, b: string, c: string) {
        return (expression: Token | CallExpression | RecursiveTokens) => {
            const el = normalOperatorLookup(expression);
            const nextSection = ++runtimeId;
            lastSection.push(
                `mov eax, [_temp${currentDeepness}]`,
                `mov ebx, ${el}`,
                `cmp eax, ebx`,
                `${a} _c0${nextSection}`,
                `jmp _c1${nextSection}`
            );
            sections["_c0" + nextSection] = [
                `mov eax, ${b}`,
                `mov [_temp${currentDeepness}], eax`,
                `jmp _c2${nextSection}`
            ];
            sections["_c1" + nextSection] = [
                `mov eax, ${c}`,
                `mov [_temp${currentDeepness}], eax`,
                `jmp _c2${nextSection}`
            ];
            lastSection = sections["_c2" + nextSection] = [];
        };
    }

    const OperatorLookup: Record<number, (expression: Token | CallExpression | RecursiveTokens) => void> = {
        [TokenType.PLUS](expression) {
            if ("value" in expression && expression.value === "1") {
                lastSection.push(
                    `mov eax, [_temp${currentDeepness}]`,
                    `inc eax`,
                    `mov [_temp${currentDeepness}], eax`
                );
                return;
            }
            const el = normalOperatorLookup(expression);
            lastSection.push(
                `mov eax, [_temp${currentDeepness}]`,
                `add eax, ${el}`,
                `mov [_temp${currentDeepness}], eax`
            );
        },
        [TokenType.MINUS](expression) {
            if ("value" in expression && expression.value === "1") {
                lastSection.push(
                    `mov eax, [_temp${currentDeepness}]`,
                    `dec eax`,
                    `mov [_temp${currentDeepness}], eax`
                );
                return;
            }
            const el = normalOperatorLookup(expression);
            lastSection.push(
                `mov eax, [_temp${currentDeepness}]`,
                `sub eax, ${el}`,
                `mov [_temp${currentDeepness}], eax`
            );
        },
        [TokenType.MULTIPLY](expression) {
            const el = normalOperatorLookup(expression);
            lastSection.push(
                `mov eax, [_temp${currentDeepness}]`,
                `imul eax, ${el}`,
                `mov [_temp${currentDeepness}], eax`
            );
        },
        [TokenType.DIVIDE](expression) {
            const el = normalOperatorLookup(expression);
            lastSection.push(
                `mov eax, [_temp${currentDeepness}]`,
                `mov ecx, ${el}`,
                `xor edx, edx`,
                `div ecx`,
                `mov [_temp${currentDeepness}], eax`
            );
        },
        [TokenType.MODULO](expression) {
            const el = normalOperatorLookup(expression);
            lastSection.push(
                `mov eax, [_temp${currentDeepness}]`,
                `mov ecx, ${el}`,
                `xor edx, edx`,
                `div ecx`,
                `mov [_temp${currentDeepness}], edx`
            );
        },
        [TokenType.EQUALS]: compareOperatorLookup("je", "1", "0"),
        [TokenType.NOT_EQUALS]: compareOperatorLookup("je", "0", "1"),
        [TokenType.GREATER_EQUAL]: compareOperatorLookup("jl", "0", "1"),
        [TokenType.SMALLER_EQUAL]: compareOperatorLookup("jg", "0", "1"),
        [TokenType.GREATER]: compareOperatorLookup("jg", "1", "0"),
        [TokenType.SMALLER]: compareOperatorLookup("jl", "1", "0"),
        [TokenType.AND](expression) {
            const el = normalOperatorLookup(expression);
            lastSection.push(
                `mov eax, [_temp${currentDeepness}]`,
                `and eax, ${el}`,
                `mov [_temp${currentDeepness}], eax`
            );
        },
        [TokenType.OR](expression) {
            const el = normalOperatorLookup(expression);
            lastSection.push(
                `mov eax, [_temp${currentDeepness}]`,
                `or eax, ${el}`,
                `mov [_temp${currentDeepness}], eax`
            );
        },
        [TokenType.XOR](expression) {
            const el = normalOperatorLookup(expression);
            lastSection.push(
                `mov eax, [_temp${currentDeepness}]`,
                `xor eax, ${el}`,
                `mov [_temp${currentDeepness}], eax`
            );
        },
    };
    for (let i = 1; i < filtered.length; i += 2) {
        const operator = <Token>filtered[i];
        const expression = filtered[i + 1];
        const look = OperatorLookup[operator.type];
        if (!look) throw new Error("Assumption failed.");
        look(expression);
    }
    return lastSection;
}

const escapes: Record<string, number> = {
    "n": 10,
    "t": 9,
    "b": 8,
    "r": 13,
    "f": 12,
    "v": 11
};

function getStrings(tokens: Token[]) {
    const strings: Record<string, number> = {};
    for (let i = 0; i < tokens.length; i++) {
        const token = tokens[i];
        if (
            token.type === TokenType.STRING
        ) {
            if (strings[token.value]) continue;
            runtimeId++;
            strings[token.value] = runtimeId;
        }
    }
    return strings;
}

function assemble(
    code: string,
    strings: Record<string, number>,
    scope: ScopeInfo,
    tokens: Token[],
    statements: Statement[],
    deepness: number,
    currentDeepness = 0,
    namedSections: Record<string, string[]>,
    lastSection: string[],
    breakSection: string | null = null,
    isInsideFunction: boolean
) {
    const EmbeddedFunctions: Record<string, Function> = {
        asm(token: Token, args: AnyExpression[][]) {
            for (let i = 0; i < args.length; i++) {
                const arg = args[i];
                if (arg.length === 1 && arg[0].type === TokenType.STRING) {
                    if (arg[0].value.includes(":")) {
                        throwError(code, token, "SyntaxError", "The ':' character cannot be used in asm() function since it can cause corruption.");
                    }
                    lastSection.push(
                        arg[0].value.slice(1, -1)
                    );
                } else throwError(code, token, "SyntaxError", "Expected a literal string for the asm() function.");
            }
        },
        malloc(token: Token, args: AnyExpression[][]) {
            // todo
        }
    };
    const StatementLookup: Record<number, Function> = {
        [ExpressionTypes.DEFINE_INT](statement: DefineIntExpression) {
            const filtered = filterExpressionAssembly(code, statement.value);
            lastSection = assembleExpression(code, filtered, strings, scope, currentDeepness, lastSection, namedSections);
            const variable = findVariable(scope, statement.name.value);
            if (!variable) throw new Error("Assumption failed.");
            lastSection.push(
                `mov eax, [_temp${currentDeepness}]`,
                `mov [_var${variable.value.id}], eax`
            );
        },
        [ExpressionTypes.IF](statement: IfStatement) {
            const filtered = filterExpressionAssembly(code, statement.if.requirement);
            lastSection = assembleExpression(code, filtered, strings, scope, currentDeepness, lastSection, namedSections);
            const id = ++runtimeId;
            lastSection.push(
                `mov eax, [_temp${currentDeepness}]`,
                `mov ebx, 0`,
                `cmp eax, ebx`,
                `je _i0${id}`,
                `jmp _i1${id}`
            );
            const continueSec: string[] = namedSections["_i2" + id] = [];
            const ifSec: string[] = namedSections["_i1" + id] = [];
            lastSection = assemble(code, strings, statement.if.scope, tokens, statements, deepness, currentDeepness + 1, namedSections, ifSec, breakSection, isInsideFunction);
            lastSection.push(`jmp _i2${id}`);
            const elseSec: string[] = namedSections["_i0" + id] = [];
            if (statement.else) {
                lastSection = assemble(code, strings, statement.else, tokens, statements, deepness, currentDeepness + 1, namedSections, elseSec, breakSection, isInsideFunction);
                lastSection.push(`jmp _i2${id}`);
            } else elseSec.push(`jmp _i2${id}`);
            lastSection = continueSec;
        },
        [ExpressionTypes.LOOP](statement: LoopStatement) {
            const id = ++runtimeId;
            const continueSec = namedSections["_l1" + id] = [];
            const loopSec = namedSections["_l0" + id] = [];
            lastSection.push(`jmp _l0${id}`);
            lastSection = assemble(code, strings, statement.scope, tokens, statements, deepness, currentDeepness + 1, namedSections, loopSec, "_l1" + id, isInsideFunction);
            lastSection.push(`jmp _l0${id}`);
            lastSection = continueSec;
        },
        [ExpressionTypes.BREAK](statement: BreakStatement) {
            if (typeof breakSection !== "string") {
                throwError(code, statement.token, "SyntaxError", "Break keyword can only be used inside loops.");
            }
            lastSection.push(`jmp ${breakSection}`);
        },
        [ExpressionTypes.RETURN](statement: ReturnStatement) {
            if (!isInsideFunction) {
                throwError(code, statement.token, "SyntaxError", "Return keyword can only be used inside functions.");
            }
            const filtered = filterExpressionAssembly(code, statement.value);
            lastSection = assembleExpression(code, filtered, strings, scope, currentDeepness, lastSection, namedSections);
            lastSection.push(
                `mov ecx, [_temp${currentDeepness}]`,
                `ret`
            );
        },
        [ExpressionTypes.FUNCTION](statement: FunctionStatement) {
            const func = scope.functions[statement.name.value];
            if (!func) throw new Error("Assumption failed.");
            const fnSec: string[] = namedSections["_func" + func.id] = [];
            const varObj: Record<string, VariableHolder> = {};
            const varL = func.definition.arguments;
            for (let i = 0; i < varL.length; i++) {
                const v = varL[i];
                varObj[v.name] = v;
            }
            const sec = assemble(code, strings, {
                parent: statement.scope,
                variables: varObj,
                functions: {},
                statements: func.definition.scope.statements,
                groups: func.definition.scope.groups
            }, tokens, statements, deepness, currentDeepness + 1, namedSections, fnSec, null, true);
            sec.push(
                `mov ecx, 0`, // return 0 if nothing is returned, ecx holds the return value
                `ret`
            );
        },
        [ExpressionTypes.STATEMENT](statement: ExpressionStatement) {
            if (statement.children.length === 0) return;
            const c1 = statement.children[0];
            const c2 = statement.children[1];
            if (c1.type === TokenType.WORD && c2 && c2.type === ExpressionTypes.PARENT) {
                const found = EmbeddedFunctions[c1.value];
                if (found) {
                    const args = splitTokensWithComma(c2.children.filter(i => i.type !== TokenType.LINE_BREAK), code);
                    found(c1, args);
                    return;
                }
            }
            const filtered = filterExpressionAssembly(code, statement.children);
            lastSection = assembleExpression(code, filtered, strings, scope, currentDeepness, lastSection, namedSections);
        }
    };
    for (let i = 0; i < scope.statements.length; i++) {
        const statement = scope.statements[i];
        const look = StatementLookup[statement.type];
        if (!look) {
            throwError(code, statement.token, "InternalError", "Unhandled token type. ID: " + statement.type);
        }
        look(statement);
    }
    return lastSection;
}

function assemblyToString(
    namedSections: Record<string, string[]>,
    strings: Record<string, number>,
    variables: VariableHolder[],
    deepness: number
) {
    const sectionData = [];
    const stringKeys = Object.keys(strings);
    for (let i = 0; i < stringKeys.length; i++) {
        const string = stringKeys[i];
        const id = strings[string];
        sectionData.push(`_${id} db ${string.slice(1, -1).split(/(\\\\|\\[nt"'brfav]|"|')/).map(i => {
            if (i === '"' || i === '\\"') return `'"'`;
            if (i === "'" || i === "\\'") return `"'"`;
            if (i.length !== 2 || i[0] !== "\\" || !(i[1] in escapes)) return "'" + i + "'";
            return escapes[i[1]];
        }).filter(i => i).join(",")}, 0`);
    }
    for (let i = 0; i < variables.length; i++) {
        const variable = variables[i];
        sectionData.push(`_var${variable.id} dd 0`); // db=8, dw=16, dd=32, dq=64 bits
    }
    for (let i = 0; i < deepness; i++) {
        sectionData.push(`_temp${i} dd 0`);
    }
    const named = [];
    let bss = [];
    const namedKeys = Object.keys(namedSections);
    for (let i = 0; i < namedKeys.length; i++) {
        const k = namedKeys[i];
        if (k === "_bss_") {
            bss.push(`section .bss`);
            bss = namedSections._bss_.map(i => "    " + i);
            continue;
        }
        named.push(
            `${k}:`,
            ...namedSections[k].map(i => "    " + i)
        )
    }
    return [
        ...bss,
        `section .data`,
        ...sectionData.map(i => "    " + i),
        `global _start`,
        ...named
    ].join("\n");
}

function cleanJunkAssembly(assembly: string) {
    return assembly
        .replace(/ +mov \[_[a-zA-Z\d]+], eax\n +mov eax, \[_[a-zA-Z\d]+]/g, match => {
            const lines = match.split("\n");
            const f1 = lines[0].trimStart().slice(5, -6);
            const f2 = lines[1].trimStart().slice(10, -1);
            if (f1 !== f2) return match;
            return match.split("\n")[0];
        });
}

function compile(code: string) {
    const tokens = tokenize(code);
    if (tokens.length === 0) {
        return "global _start\n" +
            "_start:\n" +
            "   mov eax, 1\n" +
            "   mov ebx, 0\n" +
            "   int 0x80\n";
    }
    const groups = group(code, tokens);
    const deepness = groups.deepness;
    const allStatements: Statement[] = [];
    const allVariables: VariableHolder[] = [];
    const scope = ast(code, groups, null, allStatements, allVariables);
    const strings = getStrings(tokens);
    const namedSections = {_start: []};
    const lastSection = assemble(code, strings, scope, tokens, allStatements, deepness, 0, namedSections, namedSections._start, null, false);
    lastSection.push(
        `mov eax, 1`,
        `mov ebx, 0`,
        `int 0x80`
    );
    const string = assemblyToString(namedSections, strings, allVariables, deepness);
    return cleanJunkAssembly(string);
}

function throwCliError(error: string) {
    console.error(Color.red(error));
    exit(1);
}

if (require.main === module) {
    const args: string[] = [];
    const options: Record<string, string> = {};
    for (let i = 2; i < argv.length; i++) {
        let arg = argv[i];
        if (arg.startsWith("-")) {
            if (arg.startsWith("--")) arg = arg.slice(1);
            options[arg.slice(1)] = " ";
            continue;
        }
        args.push(arg);
    }
    let file = args[0];
    if (!file) {
        throwCliError("No input file was given.");
    }
    if (!fs.existsSync(file)) {
        throwCliError("Input file not found: " + args[0]);
    }
    if (!fs.statSync(file).isFile()) {
        throwCliError("Expected a file for the input, got a directory.");
    }
    const assembly = compile(fs.readFileSync(file, "utf8"));
    const out = options.o ?? file.split(".").slice(0, -1).join(".") + ".asm";
    if (fs.existsSync(out) && !fs.statSync(out).isFile()) {
        throwCliError("Got an existing directory for the output.");
    }
    fs.writeFileSync(out, assembly);
    if (options.b || options.build) {
        const outO = out.slice(0, -3) + "o";
        const outR = out.slice(0, -4);
        child_process.execSync(`nasm -f elf ${out} -o ${outO} && ld -m elf_i386 -s -o ${outR} ${outO} && rm ${outO}`);
    }
}