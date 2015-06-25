
include Pvem
include Nonstd
module String = Sosa.Native_string

let failwithf fmt = ksprintf failwith fmt

module Exn = struct
  let to_string = Printexc.to_string
end

module Time = struct
  type t = float
  [@@deriving show, yojson]
end

module Unique_id = struct
  type t = Int64.t
  [@@deriving show, yojson]
end
