type error

val pp_error : error Fmt.t

module Method = H2.Method
module Headers = H2.Headers

type stop

val stop : unit -> stop
val switch : stop -> unit

type flow = [ `Tcp of Mnet.TCPv4.flow ]

type request = {
    meth: Method.t
  ; target: string
  ; scheme: string
  ; headers: Headers.t
}

type body = [ `V1 of H1.Body.Writer.t | `V2 of H2.Body.Writer.t ]
type reqd = [ `V1 of H1.Reqd.t | `V2 of H2.Reqd.t ]

type error_handler =
  [ `V1 | `V2 ] -> ?request:request -> error -> (Headers.t -> body) -> unit

type handler = flow -> reqd -> unit

val clear :
     ?stop:stop
  -> ?config:H1.Config.t
  -> ?ready:unit Miou.Computation.t
  -> ?error_handler:error_handler
  -> ?upgrade:(Mnet.TCPv4.flow -> unit)
  -> handler:handler
  -> Mnet.TCPv4.state
  -> port:int
  -> unit
