type spam<'a> = {
  ham: list<'a>,
}

module Play = {
  open Js.Array2
  type apples = {
    mutable spam: string,
    ham: bool,
  }
}
