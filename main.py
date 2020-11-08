
import os

from IPython.core.display import Image
from graphviz import Source, Digraph

os.environ["PATH"] += os.pathsep + 'C:\Program Files\Graphviz 2.44.1\lib'
from enum import Enum, auto

import graphviz
import pip


class Class(Enum):
    PLUS = auto()
    MINUS = auto()
    STAR = auto()
    FWDSLASH = auto()
    MOD = auto()

    OR = auto()
    AND = auto()
    NOT = auto()
    XOR = auto()

    EQ = auto()
    NEQ = auto()
    LT = auto()
    GT = auto()
    LTE = auto()
    GTE = auto()

    LPAREN = auto()
    RPAREN = auto()
    LBRACKET = auto()
    RBRACKET = auto()

    SEMICOLON = auto()
    COMMA = auto()

    TYPE = auto()
    INTEGER = auto()
    CHAR = auto()
    STRING = auto()
    ARRAY = auto()
    BOOLEAN = auto()
    REAL = auto()

    IF = auto()
    ELSE = auto()
    WHILE = auto()
    FOR = auto()
    DOWNTO = auto()
    TO = auto()
    DO = auto()
    THEN = auto()
    DIV = auto()
    OF = auto()
    BEGIN = auto()
    END = auto()
    REPEAT = auto()
    UNTIL = auto()
    VALIDACIJA = auto()

    BREAK = auto()
    CONTINUE = auto()
    VAR = auto()
    FUNCTION = auto()
    PROCEDURE = auto()
    EXIT = auto()

    TACKA = auto()
    COLON = auto()
    DVOTACKAJEDNAKO = auto()

    ID = auto()
    EOF = auto()

class Token:
    def __init__(self, class_, lexeme):
        self.class_ = class_
        self.lexeme = lexeme

    def __str__(self):
        return "<{} {}>".format(self.class_, self.lexeme)


class Lexer:
    def __init__(self, text):
        self.text = text
        self.len = len(text)
        self.pos = -1

    def read_space(self):
        while self.pos + 1 < self.len and self.text[self.pos + 1].isspace():
            self.next_char()

    def read_int(self):
        lexeme = self.text[self.pos]
        while self.pos + 1 < self.len and self.text[self.pos + 1].isdigit():
            lexeme += self.next_char()
        if self.pos + 1 < self.len and self.text[self.pos + 1] == '.' and self.text[self.pos +2].isdigit():
            lexeme += self.next_char()
            while self.pos + 1 < self.len and self.text[self.pos + 1].isdigit():
              lexeme += self.next_char()
            token = Token(Class.REAL, lexeme)
        else: token = Token(Class.INTEGER, lexeme)
        return token

    def read_char(self):
        self.pos += 1
        lexeme = self.text[self.pos]
        self.pos += 1
        return lexeme

    def read_string(self,char):
        lexeme = ''
        while self.pos + 1 < self.len and self.text[self.pos + 1] != char:
            lexeme += self.next_char()
        self.pos += 1
        return lexeme

    def read_keyword(self):
        lexeme = self.text[self.pos]
        while self.pos + 1 < self.len and self.text[self.pos + 1].isalnum():
            lexeme += self.next_char()
        if lexeme == 'if':
            return Token(Class.IF, lexeme)
        elif lexeme == 'var':
            return Token(Class.VAR, lexeme)
        elif lexeme == 'function':
            return Token(Class.FUNCTION, lexeme)
        elif lexeme == 'procedure':
            return Token(Class.PROCEDURE, lexeme)
        elif lexeme == 'else':
            return Token(Class.ELSE, lexeme)
        elif lexeme == 'while':
            return Token(Class.WHILE, lexeme)
        elif lexeme == 'for':
            return Token(Class.FOR, lexeme)
        elif lexeme == 'to':
            return Token(Class.TO, lexeme)
        elif lexeme == 'break':
            return Token(Class.BREAK, lexeme)
        elif lexeme == 'continue':
            return Token(Class.CONTINUE, lexeme)
        elif lexeme == 'exit':
            return Token(Class.EXIT, lexeme)
        elif lexeme == 'integer' or lexeme == 'char' or lexeme == 'string' or lexeme == 'boolean' or lexeme == 'real':
            return Token(Class.TYPE, lexeme)
        elif lexeme == 'begin':
            return Token(Class.BEGIN, lexeme)
        elif lexeme == 'end':
            return Token(Class.END, lexeme)
        elif lexeme == 'then':
            return Token(Class.THEN, lexeme)
        elif lexeme == 'div':
            return Token(Class.DIV, lexeme)
        elif lexeme == 'array':
            return Token(Class.ARRAY, lexeme)
        elif lexeme == 'mod':
            return Token(Class.MOD, lexeme)
        elif lexeme == 'of':
            return Token(Class.OF, lexeme)
        elif lexeme == 'downto':
            return Token(Class.DOWNTO, lexeme)
        elif lexeme == 'do':
            return Token(Class.DO, lexeme)
        elif lexeme == 'repeat':
            return Token(Class.REPEAT, lexeme)
        elif lexeme == 'until':
            return Token(Class.UNTIL, lexeme)
        elif lexeme == 'true' or lexeme == 'false':
            return Token(Class.VALIDACIJA, lexeme)
        elif lexeme == 'or':
            return Token(Class.OR, lexeme)
        elif lexeme == 'xor':
            return Token(Class.XOR, lexeme)
        elif lexeme == 'not':
            return Token(Class.NOT, lexeme)
        elif lexeme == 'and':
            return Token(Class.AND, lexeme)
        return Token(Class.ID, lexeme)

    def next_char(self):
        self.pos += 1
        if self.pos >= self.len:
            return None
        return self.text[self.pos]

    def next_token(self):
        self.read_space()
        curr = self.next_char()
        if curr is None:
            return Token(Class.EOF, curr)
        token = None
        if curr.isalpha():
            token = self.read_keyword()
        elif curr.isdigit():
            token = self.read_int()
        elif curr == '\'':
            curr = self.next_char()
            curr = self.next_char()
            self.pos -= 2
            if curr == '\'':
                token = Token(Class.CHAR, self.read_char())
            else:
                token = Token(Class.STRING, self.read_string('\''))
        elif curr == '"':
            token = Token(Class.STRING, self.read_string('"'))
        elif curr == '+':
            token = Token(Class.PLUS, curr)
        elif curr == '-':
            token = Token(Class.MINUS, curr)
        elif curr == '*':
            token = Token(Class.STAR, curr)
        elif curr == '/':
            token = Token(Class.FWDSLASH, curr)
        elif curr == '%':
            token = Token(Class.PERCENT, curr)
        elif curr == '&':
            curr = self.next_char()
            if curr == '&':
                token = Token(Class.AND, '&&')
            else:
                token = Token(Class.ADDRESS, '&')
                self.pos -= 1
        elif curr == '|':
            curr = self.next_char()
            if curr == '|':
                token = Token(Class.OR, '||')
            else:
                self.die(curr)
        elif curr == '!':
            curr = self.next_char()
            if curr == '=':
                token = Token(Class.NEQ, '!=')
            else:
                token = Token(Class.NOT, '!')
                self.pos -= 1
        elif curr == '=':
          token = Token(Class.EQ, '=')

        elif curr == '<':
            curr = self.next_char()
            if curr == '=':
                token = Token(Class.LTE, '<=')
            elif curr == '>':
                token = Token(Class.LTE, '<>')
            else:
                token = Token(Class.LT, '<')
                self.pos -= 1
        elif curr == '>':
            curr = self.next_char()
            if curr == '=':
                token = Token(Class.GTE, '>=')
            else:
                token = Token(Class.GT, '>')
                self.pos -= 1
        elif curr == '(':
            token = Token(Class.LPAREN, curr)
        elif curr == ')':
            token = Token(Class.RPAREN, curr)
        elif curr == '[':
            token = Token(Class.LBRACKET, curr)
        elif curr == ']':
            token = Token(Class.RBRACKET, curr)
        elif curr == '{':
            token = Token(Class.LBRACE, curr)
        elif curr == '}':
            token = Token(Class.RBRACE, curr)
        elif curr == ';':
            token = Token(Class.SEMICOLON, curr)
        elif curr == ',':
            token = Token(Class.COMMA, curr)
        elif curr == '.':
            token = Token(Class.TACKA, curr)
        elif curr == ':':
            curr = self.next_char()
            if curr == '=':
                token = Token(Class.DVOTACKAJEDNAKO, ":=")
            else:
                token = Token(Class.COLON, ":")
                self.pos -= 1
        else:
            self.die(curr)
        return token

    def lex(self):
        tokens = []
        while True:
            curr = self.next_token()
            tokens.append(curr)
            if curr.class_ == Class.EOF:
                break
        return tokens

    def die(self, char):
        raise SystemExit("Unexpected character: {}".format(char))
#---------------------------------------------------------------------------------------------------------------


class Node():
    pass


class Program(Node):
    def __init__(self, nodes):
        self.nodes = nodes


class Decl(Node):
    def __init__(self, type_, id_, size):
        self.type_ = type_
        self.id_ = id_
        self.size = size


class ArrayDecl(Node):
    def __init__(self, type_, id_, firstIndex, lastIndex, elems):
        self.type_ = type_
        self.id_ = id_
        self.firstIndex = firstIndex;
        self.lastIndex = lastIndex;
        self.elems = elems


class ArrayElem(Node):
    def __init__(self, id_, index):
        self.id_ = id_
        self.index = index


class DvotackaJednako(Node):
    def __init__(self, id_, expr):
        self.id_ = id_
        self.expr = expr


class If(Node):
    def __init__(self, cond, true, false):
        self.cond = cond
        self.true = true
        self.false = false


class While(Node):
    def __init__(self, cond, block):
        self.cond = cond
        self.block = block

class RepeatUntil(Node):
    def __init__(self, cond, block):
        self.cond = cond
        self.block = block

class For(Node):
    def __init__(self, init, cond, block):
        self.init = init
        self.cond = cond
        self.block = block

class Var(Node):
    def __init__(self, block):
        self.block = block


class FuncImpl(Node):
    def __init__(self, type_, id_, params, block, var):
        self.type_ = type_
        self.id_ = id_
        self.params = params
        self.block = block
        self.var = var

class ProcImpl(Node):
    def __init__(self, id_, params, block, var):
        self.id_ = id_
        self.params = params
        self.block = block
        self.var = var

class ProcCall(Node):
    def __init__(self, id_, args):
        self.id_ = id_
        self.args = args

class FuncCall(Node):
    def __init__(self, id_, args):
        self.id_ = id_
        self.args = args


class Block(Node):
    def __init__(self, nodes):
        self.nodes = nodes


class Params(Node):
    def __init__(self, params):
        self.params = params


class Args(Node):
    def __init__(self, args):
        self.args = args


class Elems(Node):
    def __init__(self, elems):
        self.elems = elems


class Break(Node):
    pass


class Continue(Node):
    pass


class Exit(Node):
    def __init__(self, expr):
        self.expr = expr


class Type(Node):
    def __init__(self, value):
        self.value = value


class Integer(Node):
    def __init__(self, value):
        self.value = value


class Char(Node):
    def __init__(self, value):
        self.value = value


class String(Node):
    def __init__(self, value):
        self.value = value

class Real(Node):
    def __init__(self, value):
        self.value = value

class Boolean(Node):
    def __init__(self, value):
        self.value = value

class Id(Node):
    def __init__(self, value):
        self.value = value


class BinOp(Node):
    def __init__(self, symbol, first, second):
        self.symbol = symbol
        self.first = first
        self.second = second


class UnOp(Node):
    def __init__(self, symbol, first):
        self.symbol = symbol
        self.first = first



from functools import wraps
import pickle

class Parser:
    def __init__(self, tokens):
        self.tokens = tokens
        self.curr = tokens.pop(0)
        self.prev = None

    def restorable(call):
        @wraps(call)
        def wrapper(self, *args, **kwargs):
            state = pickle.dumps(self.__dict__)
            result = call(self, *args, **kwargs)
            self.__dict__ = pickle.loads(state)
            return result

        return wrapper

    def eat(self, class_):
        if self.curr.class_ == class_:
            self.prev = self.curr
            self.curr = self.tokens.pop(0)
        else:
            self.die_type(class_.name, self.curr.class_.name)

    def program(self):
        nodes = []
        while self.curr.class_ != Class.EOF:
            if self.curr.class_ in [Class.BEGIN, Class.FUNCTION, Class.PROCEDURE, Class.VAR]:
                nodes.append(self.decl())
            else:
                self.die_deriv(self.program.__name__)
        return Program(nodes)

    def id_(self):
        # is_array_elem = self.prev.class_ != Class.TYPE
        id_ = Id(self.curr.lexeme)
        prev = self.prev.class_
        self.eat(Class.ID)
        if self.curr.class_ == Class.LPAREN and self.is_func_call():
            self.eat(Class.LPAREN)
            args = self.args()
            self.eat(Class.RPAREN)
            return ProcCall(id_, args)
        elif self.curr.class_ == Class.LBRACKET:
            self.eat(Class.LBRACKET)
            index = self.expr()
            self.eat(Class.RBRACKET)
            id_ = ArrayElem(id_, index)
        if self.curr.class_ == Class.DVOTACKAJEDNAKO:
            self.eat(Class.DVOTACKAJEDNAKO)
            expr = self.expr()
            return DvotackaJednako(id_, expr)
        else:
            return id_

    def decl(self):
        if self.curr.class_ == Class.VAR:
            self.eat(Class.VAR)
            block = self.block(True)
            return Var(block)
        elif self.curr.class_ == Class.PROCEDURE:
            self.eat(Class.PROCEDURE)
            id_ = self.id_()
            self.eat(Class.LPAREN)
            params = self.params()
            self.eat(Class.RPAREN)
            self.eat(Class.SEMICOLON)
            var = None
            if self.curr.class_ == Class.VAR:
                self.eat(Class.VAR)
                block = self.block(True)
                var = Var(block)
            block = None
            if self.curr.class_ == Class.BEGIN:
                self.eat(Class.BEGIN)
                block = self.block(False)
                self.eat(Class.END)
                self.eat(Class.SEMICOLON)
            return ProcImpl(id_, params, block, var)
        elif self.curr.class_ == Class.FUNCTION:
            self.eat(Class.FUNCTION)
            id_ = self.id_()
            self.eat(Class.LPAREN)
            params = self.params()
            self.eat(Class.RPAREN)
            self.eat(Class.COLON)
            type_ = self.type_()
            self.eat(Class.SEMICOLON)
            var = None
            if self.curr.class_ == Class.VAR:
                self.eat(Class.VAR)
                block = self.block(True)
                var = Var(block)
            block = None
            if self.curr.class_ == Class.BEGIN:
                self.eat(Class.BEGIN)
                block = self.block(False)
                self.eat(Class.END)
                self.eat(Class.SEMICOLON)
            return FuncImpl(type_, id_, params, block, var)
        elif self.curr.class_ == Class.BEGIN:
            self.eat(Class.BEGIN)
            block = self.block(False)
            self.eat(Class.END)
            self.eat(Class.TACKA)
            return block;
        elif self.curr.class_ == Class.ID:
            id_ = Id(self.curr.lexeme)
            self.eat(Class.ID)
            variables = []
            variables.append(id_)
            if self.curr.class_ == (Class.COMMA):
              while self.curr.class_ != Class.COLON:
                self.eat(Class.COMMA)
                id_ = Id(self.curr.lexeme)
                self.eat(Class.ID)
                variables.append(id_)
            self.eat(Class.COLON)
            if self.curr.class_ == Class.ARRAY:
                  self.eat(Class.ARRAY)
                  self.eat(Class.LBRACKET)
                  firstIndex = self.expr()
                  self.eat(Class.TACKA)
                  self.eat(Class.TACKA)
                  lastIndex = self.expr()
                  self.eat(Class.RBRACKET)
                  self.eat(Class.OF)
                  type_ = self.type_()
                  elems = None
                  if self.curr.class_ == Class.EQ:
                      self.eat(Class.EQ)
                      self.eat(Class.LPAREN)
                      elems = self.elems()
                      self.eat(Class.RPAREN)
                  self.eat(Class.SEMICOLON)
                  arrays = []
                  for v in variables:
                    arrays.append(ArrayDecl(type_, v, firstIndex, lastIndex, elems))
                  return arrays
            else:
                type_ = self.type_()
                size = None
                if type_.value == "string" and self.curr.class_ == Class.LBRACKET:
                    self.eat(Class.LBRACKET)
                    size = self.expr()
                    self.eat(Class.RBRACKET)
                self.eat(Class.SEMICOLON)
                decls = []
                for v in variables:
                   decls.append(Decl(type_, v, size))
                return decls

    def if_(self):
        self.eat(Class.IF)
        cond = self.logic()
        self.eat(Class.THEN)
        self.eat(Class.BEGIN)
        true = self.block(False)
        false = None
        self.eat(Class.END)
        if self.curr.class_ == Class.ELSE:
            self.eat(Class.ELSE)
            self.eat(Class.BEGIN)
            false = self.block(False)
            self.eat(Class.END)
        self.eat(Class.SEMICOLON)
        return If(cond, true, false)

    def while_(self):
        self.eat(Class.WHILE)
        cond = self.logic()
        self.eat(Class.DO)
        self.eat(Class.BEGIN)
        block = self.block(False)
        self.eat(Class.END)
        self.eat(Class.SEMICOLON)
        return While(cond, block)

    def for_(self):
        self.eat(Class.FOR)
        init = self.id_()
        if self.curr.class_ == Class.TO:
            self.eat(Class.TO)
        else:
            self.eat(Class.DOWNTO)
        cond = self.expr()
        self.eat(Class.DO)
        self.eat(Class.BEGIN)
        block = self.block(False)
        self.eat(Class.END)
        self.eat(Class.SEMICOLON)
        return For(init, cond, block)

    def repeat_(self):
        self.eat(Class.REPEAT)
        self.eat(Class.BEGIN)
        block = self.block(False)
        self.eat(Class.END)
        self.eat(Class.UNTIL)
        cond = self.logic()
        self.eat(Class.SEMICOLON)
        return RepeatUntil(cond, block)

    def block(self, isVar):
        nodes = []
        while self.curr.class_ != Class.END:
            print(self.curr.lexeme)
            if self.curr.class_ in [Class.BEGIN, Class.PROCEDURE, Class.FUNCTION] and isVar:
                return Block(nodes)
            elif self.curr.class_ == Class.IF:
                nodes.append(self.if_())
            elif self.curr.class_ == Class.WHILE:
                nodes.append(self.while_())
            elif self.curr.class_ == Class.FOR:
                nodes.append(self.for_())
            elif self.curr.class_ == Class.REPEAT:
                nodes.append(self.repeat_())
            elif self.curr.class_ == Class.BREAK:
                nodes.append(self.break_())
            elif self.curr.class_ == Class.CONTINUE:
                nodes.append(self.continue_())
            elif self.curr.class_ == Class.EXIT:
                nodes.append(self.exit_())
            elif self.curr.class_ == Class.ID:
                if self.jelIni():
                  nodes.append(self.id_())
                  self.eat(Class.SEMICOLON)
                else:
                  promenljiva = self.decl()
                  if isinstance(promenljiva, list):
                   while len(promenljiva) != 0:
                     nodes.append(promenljiva.pop(0))
                  else:
                    nodes.append(promenljiva)
            else:
                self.die_deriv(self.block.__name__)
        return Block(nodes)

    @restorable
    def jelIni(self):
         try:
            self.eat(Class.ID)
            if self.is_func_call():
              return True
            if self.curr.class_ == Class.LBRACKET:
              self.eat(Class.LBRACKET)
              self.expr()
              self.eat(Class.RBRACKET)
            return self.curr.class_ == Class.DVOTACKAJEDNAKO
         except:
            return False

    def params(self):
        params = []
        while self.curr.class_ != Class.RPAREN:
            if len(params) > 0:
                self.eat(Class.COMMA)
            variables = []
            arrays = []
            array = False
            if self.curr.class_ == Class.VAR:
               self.eat(Class.VAR)
               array = True
            variables.append(Id(self.curr.lexeme))
            self.eat(Class.ID)
            if self.curr.class_ == (Class.COMMA):
              while self.curr.class_ != Class.COLON:
                self.eat(Class.COMMA)
                id_ = Id(self.curr.lexeme)
                self.eat(Class.ID)
                variables.append(id_)
            self.eat(Class.COLON)
            if self.curr.class_ == Class.ARRAY:
              self.eat(Class.ARRAY)
              self.eat(Class.OF)
            type_ = self.type_()
            size = None
            if array:
              for v in variables:
               params.append(ArrayDecl(type, v,None,None,None))
            else:
              if type_.value == "string" and self.curr.class_ == Class.LBRACKET:
                    self.eat(Class.LBRACKET)
                    size = self.expr()
                    self.eat(Class.RBRACKET)
              for v in variables:
                params.append(Decl(type_, v,size))
        return Params(params)

    def args(self):
        args = []
        while self.curr.class_ != Class.RPAREN:
            if len(args) > 0:
                self.eat(Class.COMMA)
            args.append(self.expr())
        return Args(args);

    def elems(self):
        elems = []
        while self.curr.class_ != Class.RPAREN:
            if len(elems) > 0:
                self.eat(Class.COMMA)
            elems.append(self.expr())
        return Elems(elems)

    def exit_(self):
        self.eat(Class.EXIT)
        expr = self.expr()
        self.eat(Class.SEMICOLON)
        return Exit(expr)

    def break_(self):
        self.eat(Class.BREAK)
        self.eat(Class.SEMICOLON)
        return Break()

    def continue_(self):
        self.eat(Class.CONTINUE)
        self.eat(Class.SEMICOLON)
        return Continue()

    def type_(self):
        type_ = Type(self.curr.lexeme)
        self.eat(Class.TYPE)
        return type_

    def factor(self):
        if self.curr.class_ == Class.INTEGER:
            value = Integer(self.curr.lexeme)
            self.eat(Class.INTEGER)
            return value
        elif self.curr.class_ == Class.CHAR:
            value = Char(self.curr.lexeme)
            self.eat(Class.CHAR)
            return value
        elif self.curr.class_ == Class.STRING:
            value = String(self.curr.lexeme)
            self.eat(Class.STRING)
            return value
        elif self.curr.class_ == Class.REAL:
            value = Real(self.curr.lexeme)
            self.eat(Class.REAL)
            return value
        elif self.curr.class_ == Class.BOOLEAN:
            value = Boolean(self.curr.lexeme)
            self.eat(Class.BOOLEAN)
            return value
        elif self.curr.class_ == Class.ID:
            return self.id_()
        elif self.curr.class_ in [Class.MINUS, Class.NOT]:
            op = self.curr
            self.eat(self.curr.class_)
            first = None
            if self.curr.class_ == Class.LPAREN:
                self.eat(Class.LPAREN)
                first = self.logic()
                self.eat(Class.RPAREN)
            else:
                first = self.factor()
            return UnOp(op.lexeme, first)
        elif self.curr.class_ == Class.LPAREN:
            self.eat(Class.LPAREN)
            first = self.logic()
            self.eat(Class.RPAREN)
            return first
        elif self.curr.class_ == Class.SEMICOLON:
            return None
        else:
            self.die_deriv(self.factor.__name__)

    def term(self):
        first = self.factor()
        while self.curr.class_ in [Class.STAR, Class.FWDSLASH, Class.MOD, Class.DIV]:
            if self.curr.class_ == Class.STAR:
                op = self.curr.lexeme
                self.eat(Class.STAR)
                second = self.factor()
                first = BinOp(op, first, second)
            elif self.curr.class_ == Class.FWDSLASH:
                op = self.curr.lexeme
                self.eat(Class.FWDSLASH)
                second = self.factor()
                first = BinOp(op, first, second)
            elif self.curr.class_ == Class.MOD:
                op = self.curr.lexeme
                self.eat(Class.MOD)
                second = self.factor()
                first = BinOp(op, first, second)
            elif self.curr.class_ == Class.DIV:
                op = self.curr.lexeme
                self.eat(Class.DIV)
                second = self.factor()
                first = BinOp(op, first, second)
        return first

    def expr(self):
        first = self.term()
        while self.curr.class_ in [Class.PLUS, Class.MINUS]:
            if self.curr.class_ == Class.PLUS:
                op = self.curr.lexeme
                self.eat(Class.PLUS)
                second = self.term()
                first = BinOp(op, first, second)
            elif self.curr.class_ == Class.MINUS:
                op = self.curr.lexeme
                self.eat(Class.MINUS)
                second = self.term()
                first = BinOp(op, first, second)
        return first

    def compare(self):
        first = self.expr()
        if self.curr.class_ == Class.EQ:
            op = self.curr.lexeme
            self.eat(Class.EQ)
            second = self.expr()
            return BinOp(op, first, second)
        elif self.curr.class_ == Class.NEQ:
            op = self.curr.lexeme
            self.eat(Class.NEQ)
            second = self.expr()
            return BinOp(op, first, second)
        elif self.curr.class_ == Class.LT:
            op = self.curr.lexeme
            self.eat(Class.LT)
            second = self.expr()
            return BinOp(op, first, second)
        elif self.curr.class_ == Class.GT:
            op = self.curr.lexeme
            self.eat(Class.GT)
            second = self.expr()
            return BinOp(op, first, second)
        elif self.curr.class_ == Class.LTE:
            op = self.curr.lexeme
            self.eat(Class.LTE)
            second = self.expr()
            return BinOp(op, first, second)
        elif self.curr.class_ == Class.GTE:
            op = self.curr.lexeme
            self.eat(Class.GTE)
            second = self.expr()
            return BinOp(op, first, second)
        else:
            return first

    def logic(self):
        first = self.compare()
        if self.curr.class_ == Class.AND:
            op = self.curr.lexeme
            self.eat(Class.AND)
            second = self.compare()
            return BinOp(op, first, second)
        elif self.curr.class_ == Class.OR:
            op = self.curr.lexeme
            self.eat(Class.OR)
            second = self.compare()
            return BinOp(op, first, second)
        elif self.curr.class_ == Class.XOR:
            op = self.curr.lexeme
            self.eat(Class.XOR)
            second = self.compare()
            return BinOp(op, first, second)
        else:
            return first

    @restorable
    def is_func_call(self):
        try:
            self.eat(Class.LPAREN)
            self.args()
            return self.curr.class_ == Class.RPAREN
        except:
            return False

    def parse(self):
        return self.program()

    def die(self, text):
        raise SystemExit(text)

    def die_deriv(self, fun):
        self.die("Derivation error: {}".format(fun))

    def die_type(self, expected, found):
        self.die("Expected: {}, Found: {}".format(expected, found))

#----------------------------------------------------
class Visitor():
    def visit(self, parent, node):
        method = 'visit_' + type(node).__name__
        visitor = getattr(self, method, self.die)
        return visitor(parent, node)

    def die(self, parent, node):
        method = 'visit_' + type(node).__name__
        raise SystemExit("Missing method: {}".format(method))



class Grapher(Visitor):
    def __init__(self, ast):
        self.ast = ast
        self._count = 1
        self.dot = Digraph()
        self.dot.node_attr['shape'] = 'box'
        self.dot.node_attr['height'] = '0.1'
        self.dot.edge_attr['arrowsize'] = '0.5'

    def add_node(self, parent, node, name=None):
        node._index = self._count
        self._count += 1
        caption = type(node).__name__
        if name is not None:
            caption = '{} : {}'.format(caption, name)
        self.dot.node('node{}'.format(node._index), caption)
        if parent is not None:
            self.add_edge(parent, node)

    def add_edge(self, parent, node):
        src, dest = parent._index, node._index
        self.dot.edge('node{}'.format(src), 'node{}'.format(dest))

    def visit_Program(self, parent, node):
        self.add_node(parent, node)
        for n in node.nodes:
            self.visit(node, n)

    def visit_Decl(self, parent, node):
        self.add_node(parent, node)
        self.visit(node, node.type_)
        self.visit(node, node.id_)
        if node.size is not None:
          self.visit(node, node.size)

    def visit_ArrayDecl(self, parent, node):
        self.add_node(parent, node)
        self.visit(node, node.type_)
        self.visit(node, node.id_)
        if node.firstIndex is not None:
            self.visit(node, node.firstIndex)
        if node.lastIndex is not None:
            self.visit(node, node.lastIndex)
        if node.elems is not None:
            self.visit(node, node.elems)

    def visit_ArrayElem(self, parent, node):
        self.add_node(parent, node)
        self.visit(node, node.id_)
        self.visit(node, node.index)

    def visit_DvotackaJednako(self, parent, node):
        self.add_node(parent, node)
        self.visit(node, node.id_)
        self.visit(node, node.expr)

    def visit_If(self, parent, node):
        self.add_node(parent, node)
        self.visit(node, node.cond)
        self.visit(node, node.true)
        if node.false is not None:
            self.visit(node, node.false)

    def visit_While(self, parent, node):
        self.add_node(parent, node)
        self.visit(node, node.cond)
        self.visit(node, node.block)

    def visit_RepeatUntil(self, parent, node):
        self.add_node(parent, node)
        self.visit(node, node.cond)
        self.visit(node, node.block)

    def visit_For(self, parent, node):
        self.add_node(parent, node)
        self.visit(node, node.init)
        self.visit(node, node.cond)
        self.visit(node, node.block)

    def visit_FuncImpl(self, parent, node):
        self.add_node(parent, node)
        self.visit(node, node.type_)
        self.visit(node, node.id_)
        self.visit(node, node.params)
        self.visit(node, node.block)
        self.visit(node, node.var)

    def visit_ProcImpl(self, parent, node):
        self.add_node(parent, node)
        self.visit(node, node.id_)
        self.visit(node, node.params)
        self.visit(node, node.block)
        self.visit(node, node.var)

    def visit_FuncCall(self, parent, node):
        self.add_node(parent, node)
        self.visit(node, node.id_)
        self.visit(node, node.args)

    def visit_ProcCall(self, parent, node):
        self.add_node(parent, node)
        self.visit(node, node.id_)
        self.visit(node, node.args)


    def visit_Var(self, parent, node):
        self.add_node(parent, node)
        self.visit(node, node.block)

    def visit_Block(self, parent, node):
        self.add_node(parent, node)
        for n in node.nodes:
            self.visit(node, n)

    def visit_Params(self, parent, node):
        self.add_node(parent, node)
        for p in node.params:
            self.visit(node, p)

    def visit_Args(self, parent, node):
        self.add_node(parent, node)
        for a in node.args:
            self.visit(node, a)

    def visit_Elems(self, parent, node):
        self.add_node(parent, node)
        for e in node.elems:
            self.visit(node, e)

    def visit_Break(self, parent, node):
        self.add_node(parent, node)

    def visit_Continue(self, parent, node):
        self.add_node(parent, node)

    def visit_Exit(self, parent, node):
        self.add_node(parent, node)
        if node.expr is not None:
            self.visit(node, node.expr)

    def visit_Type(self, parent, node):
        name = node.value
        self.add_node(parent, node, name)

    def visit_Integer(self, parent, node):
        name = node.value
        self.add_node(parent, node, name)

    def visit_Char(self, parent, node):
        name = node.value
        self.add_node(parent, node, name)

    def visit_String(self, parent, node):
        name = node.value
        self.add_node(parent, node, name)

    def visit_Boolean(self, parent, node):
        name = node.value
        self.add_node(parent, node, name)

    def visit_Real(self, parent, node):
        name = node.value
        self.add_node(parent, node, name)

    def visit_Id(self, parent, node):
        name = node.value
        self.add_node(parent, node, name)

    def visit_BinOp(self, parent, node):
        name = node.symbol
        self.add_node(parent, node, name)
        self.visit(node, node.first)
        self.visit(node, node.second)

    def visit_UnOp(self, parent, node):
        name = node.symbol
        self.add_node(parent, node, name)
        self.visit(node, node.first)

    def graph(self):
        self.visit(None, self.ast)
        s = Source(self.dot.source, filename='graph', format='png')
        return s.view()

test_id = 1
path = f'pas/test{test_id}.pas'

with open(path, 'r') as source:
    text = source.read()

    lexer = Lexer(text)
    tokens = lexer.lex()

    parser = Parser(tokens)
    ast = parser.parse()

    grapher = Grapher(ast)
    img = grapher.graph()

Image(img)