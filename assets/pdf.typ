#set text(size: 10pt)
#set par(spacing: 0.75em)
#set list(spacing: 0.5em)
#show heading: it => block(above: 1.2em, below: .6em)[
  #set text(fill: rgb("#2a6f97"), weight: "bold", size: 11pt)
  #upper(it.body)
  #v(-.6em)
  #line(length: 100%, stroke: .5pt + rgb("#2a6f97"))
]
#show link: set text(fill: rgb("#2a6f97"))
