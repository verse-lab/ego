module Id = Id
module Basic = Basic
module Generic = struct
  module Query = Query
  module Scheduler = Scheduler
  include Language
  include Generic
end
