#!/usr/bin/env python2.7
import sys, time
from gevent import monkey, socket
monkey.patch_all()
import bson
bson.patch_socket()

s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
s.connect(("127.0.0.1", 5002))

def mongo_set(data):
    for k, v in data.iteritems():
        s.sendobj({'operation': {'operationType': {'_co': u'UFPut'}, 'operationOptions': {'payload': {'test20': [], 'test21': v}}}})
        s.recv(1024)
def mongo_get(data):
    for k in data.iterkeys():
        s.sendobj({'operation': {'operationType': {'_co': u'UFFilter'}, 'operationOptions': {'parameters': []}}})
        s.recv(1024*1024)

def do_tests(num, tests):
    # setup dict with key/values to retrieve
    data = {'key' + str(i): 'val' + str(i)*100 for i in range(num)}
    # run tests
    for test in tests:
        start = time.time()
        test(data)
        elapsed = time.time() - start
        print "Completed %s: %d ops in %.2f seconds : %.1f ops/sec" % (test.__name__, num, elapsed, num / elapsed)

if __name__ == '__main__':
    num = 1000 if len(sys.argv) == 1 else int(sys.argv[1])
    tests = [mongo_set, mongo_get] # order of tests is significant here!
    do_tests(num, tests)