import socket
import re
import string
import time
import sys

from sexp import sexpReadFromString
from sexp import sexpWriteToString

def retry(f, count, onerrorf = lambda e: (sys.stderr.write("error:"+str(e)+"\n"), time.sleep(1))):
    while True:
        try:
            return f()
        #except Exception as e:
        except Exception:
            # ugly workaround for python incompatibilities
            # see also:
            # http://stackoverflow.com/questions/11285313/try-except-as-error-in-python-2-5-python-3-x
            _, e, _ = sys.exc_info()
            try:
                onerrorf(e)
            except:
                True
            count=count-1
            if not (count > 0):
                raise

def getFunctions(host, port):
    # f=getUpsamplePL4D("localhost",2223)
    # f([[8.5,48.5],[8.6,48.5]],1000)
    # python scope or better binding sucks
    # google for "UnboundLocalError: local variable" "referenced before assignment"
    # http://mail.python.org/pipermail/python-list/2007-August/452164.html
    f=[False]
    assertProtocol = lambda rmaj,rmin: False

    def connect2():
        s=socket.create_connection((host,port))
        s.setsockopt(socket.IPPROTO_TCP, socket.TCP_NODELAY, 1)
        r=s.makefile()
        s.close()
        return r

    def connect():
        f[0]=retry(connect2, 5)
        assertProtocol(1,0)

    def reconnect():
        f[0].close()
        connect()

    def remoteEval2(s):
        f[0].write(s)
        f[0].flush()
        r=f[0].readline()
        if r=='':
            raise Exception("no result")
        # sys.stderr.write("r='"+r+"'\n")
        return r

    def remoteEval(x):
        s=sexpWriteToString(x)+"\n"
        return retry(lambda:remoteEval2(s), 5, lambda e: (sys.stderr.write("error:"+str(e)+"\n"), reconnect()))

    def assertProtocol2(rmaj,rmin):
        maj,min=parseResult(remoteEval(["protocol-version"]))
        assert maj==rmaj
        assert min==rmin

    def parseResult(s):
        r=sexpReadFromString(s)
        if len(r)>0 and r[0]=='error':
            raise Exception(r)
        return r

    assertProtocol = assertProtocol2
    connect()
    return {"z":lambda pl: parseResult(remoteEval(["z"]+pl)),
            "polyline->3d":lambda pl: parseResult(remoteEval(["polyline->3d",pl])),
            "upsample-polyline->4d":lambda pl, dist: parseResult(remoteEval(["upsample-polyline->4d","wgs84",pl,dist])),
            "sample-polyline->4d":lambda pl, count: parseResult(remoteEval(["sample-polyline->4d","wgs84",pl,count]))}
            
