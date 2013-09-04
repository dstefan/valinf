#
# Outcomes
#
Cost = function(s, p) {
  return(
    Choose.one("Cost", s, p)
  )
}

Market.size = function(s, p) {
  return(
    Choose.one("Market.size", s, p)
  )
}

Market.share = function(s, p) {
  return(
    Choose.one("Market.share", s, p)
  )
}

Benefit = function(s, p) {
  return(
    Market.size(s, p) * Market.share(s, p) / 100
  )
}