-- Remove the top-level heading since Haddock in itself prints it.
function Header(elem)
  if elem.level > 1 then
    return elem
  end
  return {}
end

-- Removes linked images since Haddock cannot represent them.
function Link(elem)
  children = elem.content
  if #children ~= 1 or children[1].tag ~= "Image" then
      return nil
  end
  return {}
end

-- Escape slashes in hrefs of emphasized links as Pandoc's Haddock target
-- does not escape slashes for us.
function Emph(elem)
  return pandoc.walk_inline(elem, {
    Link = function (elem)
      elem.target = string.gsub(elem.target, "/", "\\/")
      return elem
    end
  })
end
