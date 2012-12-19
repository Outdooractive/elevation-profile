function get_upsample_pl4d(host, port)
   local socket = require("socket")
   local client = assert(socket.tcp())
   local assert_protocol

   local function connect()
      local retry=5
      while not client:connect(socket.dns.toip(host), port) do
	 -- print("connect failed retry="..retry)
	 retry=retry-1
	 if retry==0 then
	    return false
	 end
	 socket.sleep(1)
      end
      assert(client:setoption("tcp-nodelay", true)==1)
      -- print("connected")
      assert_protocol(1,0)
      return true
   end

   local function reconnect()
      client:close()
      client = assert(socket.tcp())
      assert(connect())
   end

   local function readline()
      local str
      local err
      local part
      str, err, part = client:receive('*l')
      return str
   end
   
   local function remote_eval(s)
      local retry=5
      local ret=false
      while true do
	 if client:send(s) then
	    -- todo: lua socket implementation not ideal?
	    -- strace shows receive+select+receive
	    ret=readline()
	    if ret then
	       return ret
	    end
	 end
	 -- print("send or read failed retry="..retry)
	 retry=retry-1
	 assert(retry>0)
	 reconnect()
      end
   end

   local function map(func, array)
      local new_array = {}
      for i,v in ipairs(array) do
	 new_array[i] = func(v)
      end
      return new_array
   end

   local function string_prefix_p(p,s)
      return string.sub(s,1,#p)==p
   end

   local function parse_result(s)
      local r={}
      -- check for error
      assert(not string_prefix_p("(error", s))
      for i in string.gmatch(s, "%([^%)]+%)") do
	 local p={}
	 -- todo: crap
	 for j in string.gmatch(i, "[^%(%) ]+") do
	    table.insert(p,j)
	 end
	 table.insert(r,p)
      end
      return r
   end

   assert_protocol=function(rmaj,rmin)
      -- print("check protocol version")
      local protomaj,protomin=string.gmatch(remote_eval("(protocol-version)\n"), "%((%d+) (%d+)%)")()
      assert(0+protomaj==rmaj)
      assert(0+protomin>=rmin)
   end
   
   assert(connect())

   return function(pl,dist)
      return parse_result(remote_eval("(upsample-polyline->4d wgs84 ("..table.concat(map(function(x) return "("..table.concat(x," ")..")" end, pl)," ")..") "..dist..")\n"))
   end
end
