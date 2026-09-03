let src = Logs.Src.create "mhttp"

module Log = (val Logs.src_log src : Logs.LOG)

external reraise : exn -> 'a = "%reraise"

module TCP = struct
  type t = Mnet.TCP.buffer Mnet.TCP.flow

  let read flow bstr ~off ~len = Mnet.TCP.read_bigarray flow ~off ~len bstr

  (* NOTE(dinosaure): There is an important subtlety here that needs to be
     understood between httpcats and mhttp/mnet/utcp. Currently, mnet/utcp is
     not blocking when writing. This means that the string we give is probably
     kept in the utcp state (cached) and will actually be sent during a tick.

     On the other hand, the buffers of the [Faraday] iovecs belong to
     httpcats.runtime, which reuses them as soon as [writev] returns. So we
     must hand utcp something it can keep: [Bstr.sub_string] already builds a
     fresh immutable string, which is exactly one copy - the previous
     implementation had two (the iovec was first blitted into a scratch [bytes]
     owned by the runtime, and that scratch had to be copied again here). *)
  let writev flow bstrs =
    let fn { Faraday.buffer; off; len } =
      Mnet.TCP.write flow (Bstr.sub_string buffer ~off ~len)
    in
    List.iter fn bstrs

  let close = Mnet.TCP.close
  let shutdown = Mnet.TCP.shutdown
end

module TLS = Runtime.Flow.Of_bytes (struct
  include Mnet_tls

  let write fd ?off ?len str =
    try write fd ?off ?len str with
    | Mnet_tls.Closed_by_peer -> reraise Runtime.Flow.Closed_by_peer
    | exn -> reraise exn
end)
