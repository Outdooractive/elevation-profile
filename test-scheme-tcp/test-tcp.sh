#!/bin/bash -x
socat TCP4-LISTEN:10000,reuseaddr,bind=127.0.0.1,nodelay,fork EXEC:"./worker.sh",nofork &
DPID="$!"
./test-scheme-tcp.scm &
CPID="$!"
while ! ps h -o pid --ppid $DPID; do
    sleep 1
done
kill -SIGINT $(ps h -o pid --ppid $DPID) || true
wait $CPID
RET="$?"
kill $DPID
wait || true
exit $RET
