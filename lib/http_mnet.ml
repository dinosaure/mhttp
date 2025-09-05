let src = Logs.Src.create "http-mkernel"

module Log = (val Logs.src_log src : Logs.LOG)

module TCP = struct
  type t = Mnet.TCPv4.flow

  let read = Mnet.TCPv4.read
  let write = Mnet.TCPv4.write
  let close = Mnet.TCPv4.close
  let shutdown = Mnet.TCPv4.shutdown
end
