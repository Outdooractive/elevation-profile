#!/usr/bin/python
import dem
def test():
    upsample_pl4d=dem.getFunctions("127.0.0.1",10000)["upsample-polyline->4d"]
    for i in range(5000):
        assert len(upsample_pl4d([[8.5,48.5],[8.6,48.5]],1000))==9

test()
