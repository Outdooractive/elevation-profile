#!/usr/bin/lua
require('elpro-tcp')
function test()
   local upsample_pl4d=get_upsample_pl4d("127.0.0.1",10000)
   for i=1,5000 do
      assert(table.maxn(upsample_pl4d({{8.5,48.5},{8.6,48.5}},1000))==9)
   end
end

if not pcall(test) then
   os.exit(1)
end
