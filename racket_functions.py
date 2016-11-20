from collections import namedtuple
from functools import reduce
import racython

def racket_list(*args):
    return list(args)

def racket_cons(first, rest):
    rest.insert(0,first)
    return rest

def racket_empty_huh(list):
    return not bool(list)

def racket_or(*args):
    return any(args)

def racket_and(*args):
    return all(args)

def racket_not(bool):
    return not bool

def racket_first(list):
    return list[0]

def racket_second(list):
    return list[1]

def racket_third(list):
    return list[2]

def racket_rest(list):
    return list[1:]

def racket_reverse(list):
    return list[::-1]

def racket_make_struct(args):
    arg = ' '.join(args[1])
    return namedtuple(args[0], arg)

def racket_access_struct(index):
    return lambda nt: nt[index]

def racket_struct_huh(nt):
    return lambda int: isinstance(int, nt)

def racket_map(function, list):
    return [racython.apply(function,[x]) for x in list]

def racket_filter(function, lst):
    return list(filter(lambda x: racython.apply(function, [x]), lst))

def racket_integerToChar(int):
    return chr(int)

def racket_foldl(function, base, lst):
    return reduce(lambda x,acc: racython.apply(function, [x, acc]), lst, base)

def racket_foldr(function, base, lst):
    acc = base
    for elem in lst[::-1]:
        acc = racython.apply(function, [elem, acc])
    return acc

def racket_andmap(function, lst):
    for elem in lst:
        if function(elem) == False:
            return False
    return True

def racket_ormap(function, lst):
    for elem in lst:
        if function(elem) == True:
            return True
    return False

def racket_checkExpect(arg1, arg2):
    assert arg1 == arg2, "check-expect failed. " + str(arg1) + " doesn't equal " + str(arg2)

def racket_display(inStr):
    print(inStr)

def racket_begin(*args):
    return args[-1]

def racket_read():
    return input()

def racket_listToString(list):
    return ''.join(list)

def racket_error(*args):
    class RacketRaisedError(Exception):
        pass
    raise RacketRaisedError(' '.join(args))

def racket_explode(str):
    return list(str)

def racket_buildList(num, func):
    return [racython.apply(func, [x]) for x in range(num)]

def racket_length(list):
    return len(list)

def racket_listRef(list, num):
    return list[num]

def racket_add1(num):
    return num+1

def racket_sub1(num):
    return num-1

def racket_modulo(num1, num2):
    return num1%num2