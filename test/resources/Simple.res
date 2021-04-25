@bs.val external document: {..} = "document"

// Comment

let style = document["createElement"]("style")
document["head"]["appendChild"](style)
style["innerHTML"] = ExampleStyles.style

let makeContainer = _ => {
  let container = document["createElement"]("div")
  let () = document["body"]["appendChild"](container)
  container
}
