
D = list(
  #
  # List of deicisons with respective alternatives
  #
  
  "Loc.finding" = list(
    "GPS", "Radio"
  ),
  "HW.platform" = list(
    "Nexus1", "Motorola"
  ),
  "File.sharing" = list(
    "File.mgr", "In.house"
  ),
  "Report.sync" = list(
    "Explicit", "Implicit"
  ),
  "Chat" = list(
    "Openfire", "In.house"
  ),
  "Map.access" = list(
    "On.demand", "Cached", "Preloaded"
  ),
  "Connectivity" = list(
    "Wifi", "3G.nexus", "3g.droid", "Bluetooth"
  ),
  "DB" = list(
    "MySQL", "SQLLite"
  ),
  "Arch.style" = list(
    "P2P", "Client.server", "Push.based"
  ),
  "Data.exchange" = list(
    "XML", "Compressed.xml", "Uniform.data"
  )
)

G = list(
  #
  # List of goals with respective evaluation functions
  #
  
  Ramp.up.time = function(s, w) {
    return(Loc.finding + HW.platform + File.sharing + Report.sync + Chat + Map.access + Connectivity + DB + Arch.style + Data.exchange)
  },
  Dev.cost = function() {
    return(Loc.finding + HW.platform + File.sharing + Report.sync + Chat + Map.access + Connectivity + DB + Arch.style + Data.exchange)
  },
  Dev.time = function() {
    return(Loc.finding + HW.platform + File.sharing + Report.sync + Chat + Map.access + Connectivity + DB + Arch.style + Data.exchange)
  },
  Dep.time = function() {
    return(Loc.finding + HW.platform + File.sharing + Report.sync + Chat + Map.access + Connectivity + DB + Arch.style + Data.exchange)
  },  
  Battery.life = function() {
    return(Loc.finding + HW.platform + File.sharing + Report.sync + Chat + Map.access + Connectivity + DB + Arch.style + Data.exchange)
  },
  Resp.time = function() {
    return(Loc.finding + HW.platform + File.sharing + Report.sync + Chat + Map.access + Connectivity + DB + Arch.style + Data.exchange)
  },
  Reliability = function() {
    return(Loc.finding + HW.platform + File.sharing + Report.sync + Chat + Map.access + Connectivity + DB + Arch.style + Data.exchange)
  }
)

Cost = function(s, w) {
  #
  # Cost function. Defined using calls to the g function, a schortcut for the
  # eval function that evaluates goal for given solution s.
  #
  
  return(g("Ramp.up.time", s, w) + g("Dev.cost", s, w) + g("Dev.time", s, w) + g("Dep.time", s, w))
}

Utility = function(s, w) {
  #
  # Utility function. Defined using calls to the g function, a schortcut for the
  # eval function that evaluates goal for given solution s.
  #
  
  return(g("Battery.life", s, w) + g("Reliability", s, w) - g("Resp.time", s, w))
}

Profit = function(s, w) {
  #
  # Profit function.
  #
  
  return(Utility(s, w) - Cost(s, w))
}

