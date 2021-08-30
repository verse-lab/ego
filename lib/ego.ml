module Id = Id
module Basic = Basic
module Generic = struct
  module Query = Query
  include Language
  include Generic
end
