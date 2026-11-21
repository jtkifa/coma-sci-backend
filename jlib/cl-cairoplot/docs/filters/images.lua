-- Show each picture at the size of the plot it shows. The pictures are made at twice
-- the size, for sharp text on high-resolution screens, so their width in pixels is
-- halved; Pandoc turns px into the right size for HTML and for the PDF (at --dpi=96).

local function png_width(path)
  local f = io.open(path, "rb")
  if not f then return nil end
  local header = f:read(24)
  f:close()
  if not header or header:sub(2, 4) ~= "PNG" then return nil end
  local b1, b2, b3, b4 = header:byte(17, 20)
  return ((b1 * 256 + b2) * 256 + b3) * 256 + b4
end

function Image(image)
  if not image.attributes.width then
    local width = png_width(image.src)
    if width then image.attributes.width = tostring(math.floor(width / 2)) .. "px" end
  end
  return image
end
