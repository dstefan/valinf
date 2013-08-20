#
# Outcomes
#
Ramp.up.time = function(s, p) {
  return(
    Loc.finding("Ramp.up.time", s, p) + HW.platform("Ramp.up.time", s, p) + File.sharing("Ramp.up.time", s, p) + Report.sync("Ramp.up.time", s, p) + Chat("Ramp.up.time", s, p) + Map.access("Ramp.up.time", s, p) + Connectivity("Ramp.up.time", s, p) + DB("Ramp.up.time", s, p) + Arch.style("Ramp.up.time", s, p) + Data.exchange("Ramp.up.time", s, p)
  )
}

Dev.cost = function(s, p) {
  return(
    Loc.finding("Dev.cost", s, p) + HW.platform("Dev.cost", s, p) + File.sharing("Dev.cost", s, p) + Report.sync("Dev.cost", s, p) + Chat("Dev.cost", s, p) + Map.access("Dev.cost", s, p) + Connectivity("Dev.cost", s, p) + DB("Dev.cost", s, p) + Arch.style("Dev.cost", s, p) + Data.exchange("Dev.cost", s, p)
  )
}

Dev.time = function(s, p) {
  return(
    Loc.finding("Dev.time", s, p) + HW.platform("Dev.time", s, p) + File.sharing("Dev.time", s, p) + Report.sync("Dev.time", s, p) + Chat("Dev.time", s, p) + Map.access("Dev.time", s, p) + Connectivity("Dev.time", s, p) + DB("Dev.time", s, p) + Arch.style("Dev.time", s, p) + Data.exchange("Dev.time", s, p)
  )
}

Dep.time = function(s, p) {
  return (
    Loc.finding("Dep.time", s, p) + HW.platform("Dep.time", s, p) + File.sharing("Dep.time", s, p) + Report.sync("Dep.time", s, p) + Chat("Dep.time", s, p) + Map.access("Dep.time", s, p) + Connectivity("Dep.time", s, p) + DB("Dep.time", s, p) + Arch.style("Dep.time", s, p) + Data.exchange("Dep.time", s, p)
  )
}

Battery.usage = function(s, p) {
  return(
    Loc.finding("Battery.usage", s, p) + HW.platform("Battery.usage", s, p) + File.sharing("Battery.usage", s, p) + Report.sync("Battery.usage", s, p) + Chat("Battery.usage", s, p) + Map.access("Battery.usage", s, p) + Connectivity("Battery.usage", s, p) + DB("Battery.usage", s, p) + Arch.style("Battery.usage", s, p) + Data.exchange("Battery.usage", s, p)
  )
}

Resp.time = function(s, p) {
  return(
    Loc.finding("Resp.time", s, p) + HW.platform("Resp.time", s, p) + File.sharing("Resp.time", s, p) + Report.sync("Resp.time", s, p) + Chat("Resp.time", s, p) + Map.access("Resp.time", s, p) + Connectivity("Resp.time", s, p) + DB("Resp.time", s, p) + Arch.style("Resp.time", s, p) + Data.exchange("Resp.time", s, p)
  )
}

Reliability = function(s, p) {
  return(
    100 - row.min(cbind(Loc.finding("Reliability", s, p), File.sharing("Reliability", s, p), Report.sync("Reliability", s, p), Chat("Reliability", s, p), Map.access("Reliability", s, p), Connectivity("Reliability", s, p), DB("Reliability", s, p), Arch.style("Reliability", s, p)))
  )
}


#
# Goals
#
goal.Dep.time = function(s, p) {
  must = 45
  target = 30
  dep.time = Dep.time(s, p)
  res = (dep.time - must) / (target - must); res[res < 0] = 0; res[res > 1] = 1;
  return ((dep.time - must) / (target - must))
}

goal.Battery.usage= function(s, p) {
  must = 65
  target = 55
  battery.usage = Battery.usage(s, p)
  res = (battery.usage - must) / (target - must); res[res < 0] = 0; res[res > 1] = 1;
  return ((battery.usage - must) / (target - must))
}

goal.Resp.time = function(s, p) {
  must = 1800
  target = 1200
  resp.time = Resp.time(s, p)
  res = (resp.time - must) / (target - must); res[res < 0] = 0; res[res > 1] = 1;
  return(res)
}

goal.Reliability = function(s, p) {
  must = 36
  target = 32
  reliability = Reliability(s, p)
  res = (reliability - must) / (target - must); res[res < 0] = 0; res[res > 1] = 1;
  return(res)
}

#
# Cost/utility model
#
Cost = function(s, p) {
  return(
    15 * Ramp.up.time(s, p) + Dev.cost(s, p) + 15 * Dev.time(s, p)
  )
}

Utility = function(s, p) {
  return(2 * goal.Dep.time(s, p) + 9 * goal.Battery.usage(s, p) + 7 * goal.Resp.time(s, p) + 3 * goal.Reliability(s, p))
}

Profit = function(k, s, p) {
  return(
    k * Utility(s, p) - Cost(s, p)
  )
}
