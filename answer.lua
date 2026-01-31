-- answer.lua
function Div(el)
  if el.classes:includes("answer") then
    local blocks = {}
    table.insert(blocks, pandoc.RawBlock("latex", "\\begin{answer}"))
    for _, b in ipairs(el.content) do
      table.insert(blocks, b)
    end
    table.insert(blocks, pandoc.RawBlock("latex", "\\end{answer}"))
    return blocks
  end
end

