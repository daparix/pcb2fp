#!/usr/bin/env python

import ply.lex as lex
import ply.yacc as yacc
import sys

DEBUGLEVEL = 0

def debug(level, s):
    if level <= DEBUGLEVEL:
        print s

# ================================================ LEX ====================================================

tokens = [
    'LPAR', 'RPAR', 'LCOR', 'RCOR',
    'STRING','N_MIL', 'N_MM', 'NUM',
    'COMMENT', 'OTHER',
    'LAYER', 'ELEMENT',
    'PIN', 'PAD', 'ELINE', 'EARC', 'LINE', 'VIA',
    ]


def t_COMMENT(t):
    r'\#[ -~]*\n'
    t.lexer.lineno += 1
    debug(4, '# COMMENT('+t.value+')')
    pass

def t_LPAR(t):
    r'\('
    debug(3, '# (')
    return t

def t_RPAR(t):
    r'\)'
    debug(3, '# )')
    return t

def t_LCOR(t):
    r'\['
    debug(3, '# [')
    return t

def t_RCOR(t):
    r'\]'
    debug(3, '# ]')
    return t

def t_LAYER(t):
    r'Layer'
    debug(3, '# LAYER')
    return t

def t_PIN(t):
    r'Pin'
    debug(3, '# PIN')
    return t

def t_PAD(t):
    r'Pad'
    debug(3, '# PAD')
    return t

def t_ELINE(t):
    r'ElementLine'
    debug(3, '# ELEMENT-LINE')
    return t

def t_EARC(t):
    r'ElementArc'
    debug(3, '# ELEMENT-ARC')
    return t

def t_ELEMENT(t):
    r'Element'
    debug(3, '# ELEMENT')
    return t

def t_LINE(t):
    r'Line'
    debug(3, '# LINE')
    return t

def t_VIA(t):
    r'Via'
    debug(3, '# VIA')
    return t

def t_STRING(t):
    r'\"[$-~! ]*\"'
    debug(3, '# STRING('+t.value+')')
    return t

def t_N_MIL(t):
    r'-?\d+(\.\d*)?mil'
    try:
        l = t.value.split('.')
        s = l[1].split('m')[0]
        if len(s) < 2:
            s += '0'
        if l[0][0]!='-':
            sign = +1
        else:
            sign = -1
        t.value = int(l[0])*100+sign*int(s[:2])
    except ValueError:
        debug(2, "# NUM-MIL: Value unknown " + str(t.value))
        t.value = 0
    debug(3, '# NUM-MIL('+str(t.value)+')')
    return t

def t_N_MM(t):
    r'-?\d+(\.\d*)?mm'
    try:
        l = t.value.split('.')
        s = l[1].split('m')[0]
        if len(s) < 2:
            s += '0000'
        elif len(s) < 3:
            s += '000'
        elif len(s) < 4:
            s += '00'
        elif len(s) < 5:
            s += '0'
        if l[0][0]!='-':
            sign = +1
        else:
            sign = -1
        t.value = int(l[0])*100000+sign*int(s[:5])
    except ValueError:
        debug(2, "# NUM-MM: Value unknown " + str(t.value))
        t.value = 0
    debug(3, '# NUM-MM('+str(t.value)+')')
    return t

def t_NUM(t):
    r'-?\d+'
    try:
        t.value = int(t.value)
    except ValueError:
        debug(2, "# NUM-CMIL: Value unknown "+ str(t.value))
        t.value = 0
    debug(3, '# NUM-CMIL('+str(t.value)+')')
    return t

def t_OTHER(t):
    r'[#-~!]+'
    debug(4, '# OTHER('+str(t.value)+')')
    pass

# Ignored characters
t_ignore = " \t"

def t_newline(t):
    r'\n'
    debug(4, '# NEWLINE')
    t.lexer.lineno += 1
    
def t_error(t):
    debug(4, "# Illegal char '" + t.value[0] + "'")
    t.lexer.skip(1)


# ================================================ YACC ====================================================

start = 'list0'

class Expr: pass

class Pin(Expr):
    def __init__(self, x0, y0, thick, clear, mask, drill, name, number, flag):
        self.type   = "PIN"
        self.x0     = int(x0)
        self.y0     = int(y0)
        self.thick  = int(thick)
        self.clear  = int(clear)
        self.mask   = int(mask)
        self.drill  = int(drill)
        self.name   = str(name)
        self.number = str(number)
        self.flags  = str(flag)

class Pad(Expr):
    def __init__(self, x0, y0, x1, y1, thick, clear, mask, name, number, flag):
        self.type   = "PAD"
        self.x0     = int(x0)
        self.y0     = int(y0)
        self.x1     = int(x1)
        self.y1     = int(y1)
        self.thick  = int(thick)
        self.clear  = int(clear)
        self.mask   = int(mask)
        self.name   = str(name)
        self.number = str(number)
        self.flags  = str(flag)

class ElementLine(Expr):
    def __init__(self, x0, y0, x1, y1, thick):
        self.type   = "ELINE"
        self.x0     = int(x0)
        self.y0     = int(y0)
        self.x1     = int(x1)
        self.y1     = int(y1)
        self.thick  = int(thick)

class ElementArc(Expr):
    def __init__(self, x0, y0, width, height, angle0, angle1, thick):
        self.type   = "EARC"
        self.x0     = int(x0)
        self.y0     = int(y0)
        self.width  = int(width)
        self.height = int(height)
        self.angle0 = int(angle1)
        self.angle1 = int(angle1)
        self.thick  = int(thick)


def p_empty(p):
    'empty :'
    pass

def p_list0n(t):
    'list0 : list0 element0'
    t[0] = t[1] + [t[2]]
    
def p_list0(t):
    'list0 : empty'
    t[0] = []
    
def p_list1n(t):
    'list1 : list1 element1'
    t[0] = t[1] + [t[2]]
    
def p_list1(t):
    'list1 : empty'
    t[0] = []
    
def p_list2n(t):
    'list2 : list2 element2'
    t[0] = t[1] + [t[2]]
    
def p_list2(t):
    'list2 : empty'
    t[0] = []
    
def p_element(t):
    'element0 : ELEMENT LCOR STRING STRING STRING STRING num num num num num num STRING RCOR LPAR list1 RPAR'
    debug(1, "# parser:element %s,%s,%s" % (t[1],t[7],t[8]))
    x = int(t[7])
    y = int(t[8])
    for i in t[16]:
        if i.type == 'PIN':
            print "    Pin[{} {} {} {} {} {} {} {} {}]".format(i.x0+x, i.y0+y, i.thick, i.clear, i.mask, i.drill, i.name, i.number, i.flags)
        if i.type == 'PAD':
            print "    Pad[{} {} {} {} {} {} {} {}]".format(i.x0+x, i.y0+y, i.thick, i.clear, i.mask, i.name, i.number, i.flags)
        if i.type == 'ELINE':
            print "    ElementLine[{} {} {} {} {}]".format(i.x0+x, i.y0+y, i.x1+x, i.y1+y, i.thick)
        if i.type == 'EARC':
            print "    ElementArc[{} {} {} {} {} {} {}]".format(i.x0+x, i.y0+y, i.width, i.height, i.angle0, i.angle1, i.thick)
    t[0] = []

def p_layer(t):
    'element0 : LAYER LPAR num STRING RPAR LPAR list2 RPAR'
    if t[4]=='"top"' or t[4]=='"silk"':
        for i in t[7]:
            if i.type == 'ELINE':
                print "    ElementLine[{} {} {} {} {}]".format(i.x0, i.y0, i.x1, i.y1, i.thick)
    t[0] = []

def p_via(t):
    'element0 : VIA LCOR num num num num num num STRING STRING RCOR'
    print "    Pin[{} {} {} {} {} {} {} {} {}]".format(t[3], t[4], t[5], t[6], t[7], t[8], t[9], '"0"', t[10])
    t[0] = []

def p_other(t):
    '''element0 : STRING
                | num
                | LPAR
                | RPAR
                | LCOR
                | RCOR
                | COMMENT
                | OTHER'''
    t[0] = []

def p_element1_pin(t):
    'element1 : PIN LCOR num num num num num num STRING STRING STRING RCOR'
    t[0] = Pin(t[3], t[4], t[5], t[6], t[7], t[8], t[9], t[10], t[11])

def p_element1_pad(t):
    'element1 : PAD LCOR num num num num num num num STRING STRING STRING RCOR'
    t[0] = Pad(t[3], t[4], t[5], t[6], t[7], t[8], t[9], t[10], t[11], t[12])
    
def p_element1_eline(t):
    'element1 : ELINE LCOR num num num num num RCOR'
    t[0] = ElementLine(t[3], t[4], t[5], t[6], t[7])

def p_element1_earc(t):
    'element1 : EARC LCOR num num num num num num num RCOR'
    t[0] = ElementArc(t[3], t[4], t[5], t[6], t[7], t[8], t[9])

def p_element2_line(t):
    'element2 : LINE LCOR num num num num num num STRING RCOR'
    t[0] = ElementLine(t[3], t[4], t[5], t[6], t[7])

def p_num(t):
    '''num : NUM
           | N_MIL
           | N_MM'''
    t[0] = t[1]

def p_error(t):
    print("Syntax error at '%s' #%i" % (t.value, t.lineno) )
    exit(0)

def main(argv):
  if len(argv)<2:
      print "Usage is " + argv[0] + " <file.pcb>"
      exit(0)
  lexer = lex.lex()

  parser = yacc.yacc(debug=True)
  with open(argv[1]) as f:
      print '# component generated with {}'.format(argv[0])
      print 'Element["" "" "" "" 0 0 0 0 0 100 ""]'
      print "("
#      for line in f:
#          print line
#          lex.input(line)
#          for tok in iter(lex.token, None):
#              print repr(tok.type), repr(tok.value)
#          parser.parse(line)
      parser.parse(f.read())
      print ")"
  

if __name__ == "__main__":
    main(sys.argv)

