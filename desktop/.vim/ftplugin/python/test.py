class A:
    def a(self):
        print self._b

def bar(self):
    print 42

A._b = '1'
#A.a = bar
foo = A()
foo.a()
