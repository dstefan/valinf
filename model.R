
#
# Outputs
#

Ramp.up.time = function(s, w) {
  SUM = Loc.finding("Ramp.up.time", s, w) + HW.platform("Ramp.up.time", s, w) + File.sharing("Ramp.up.time", s, w) + Report.sync("Ramp.up.time", s, w) + Chat("Ramp.up.time", s, w) + Map.access("Ramp.up.time", s, w) + Connectivity("Ramp.up.time", s, w) + DB("Ramp.up.time", s, w) + Arch.style("Ramp.up.time", s, w) + Data.exchange("Ramp.up.time", s, w)
  return(
    1 - (SUM - MIN["Ramp.up.time"]) / (MAX["Ramp.up.time"] - MIN["Ramp.up.time"])
  )
}

Dev.cost = function(s, w) {
  SUM = Loc.finding("Dev.cost", s, w) + HW.platform("Dev.cost", s, w) + File.sharing("Dev.cost", s, w) + Report.sync("Dev.cost", s, w) + Chat("Dev.cost", s, w) + Map.access("Dev.cost", s, w) + Connectivity("Dev.cost", s, w) + DB("Dev.cost", s, w) + Arch.style("Dev.cost", s, w) + Data.exchange("Dev.cost", s, w)
  return(
    1 - (SUM - MIN["Dev.cost"]) / (MAX["Dev.cost"] - MIN["Dev.cost"])
  )
}

Dev.time = function(s, w) {
  SUM = Loc.finding("Dev.time", s, w) + HW.platform("Dev.time", s, w) + File.sharing("Dev.time", s, w) + Report.sync("Dev.time", s, w) + Chat("Dev.time", s, w) + Map.access("Dev.time", s, w) + Connectivity("Dev.time", s, w) + DB("Dev.time", s, w) + Arch.style("Dev.time", s, w) + Data.exchange("Dev.time", s, w)
  return(
    1 - (SUM - MIN["Dev.time"]) / (MAX["Dev.time"] - MIN["Dev.time"])
  )
}

Dep.time = function(s, w) {
  SUM = Loc.finding("Dep.time", s, w) + HW.platform("Dep.time", s, w) + File.sharing("Dep.time", s, w) + Report.sync("Dep.time", s, w) + Chat("Dep.time", s, w) + Map.access("Dep.time", s, w) + Connectivity("Dep.time", s, w) + DB("Dep.time", s, w) + Arch.style("Dep.time", s, w) + Data.exchange("Dep.time", s, w)
  return(
    1 - (SUM - MIN["Dep.time"]) / (MAX["Dep.time"] - MIN["Dep.time"])
  )
}

Battery.usage = function(s, w) {
  SUM = Loc.finding("Battery.usage", s, w) + HW.platform("Battery.usage", s, w) + File.sharing("Battery.usage", s, w) + Report.sync("Battery.usage", s, w) + Chat("Battery.usage", s, w) + Map.access("Battery.usage", s, w) + Connectivity("Battery.usage", s, w) + DB("Battery.usage", s, w) + Arch.style("Battery.usage", s, w) + Data.exchange("Battery.usage", s, w)
  return(
    1 - (SUM - MIN["Battery.usage"]) / (MAX["Battery.usage"] - MIN["Battery.usage"])
  )
}

Resp.time = function(s, w) {
  SUM = Loc.finding("Resp.time", s, w) + HW.platform("Resp.time", s, w) + File.sharing("Resp.time", s, w) + Report.sync("Resp.time", s, w) + Chat("Resp.time", s, w) + Map.access("Resp.time", s, w) + Connectivity("Resp.time", s, w) + DB("Resp.time", s, w) + Arch.style("Resp.time", s, w) + Data.exchange("Resp.time", s, w)
  return(
    1 - (SUM - MIN["Resp.time"]) / (MAX["Resp.time"] - MIN["Resp.time"])
  )
}

Reliability = function(s, w) {
  SUM = Loc.finding("Reliability", s, w) + HW.platform("Reliability", s, w) + File.sharing("Reliability", s, w) + Report.sync("Reliability", s, w) + Chat("Reliability", s, w) + Map.access("Reliability", s, w) + Connectivity("Reliability", s, w) + DB("Reliability", s, w) + Arch.style("Reliability", s, w) + Data.exchange("Reliability", s, w)
  return(
    (SUM - MIN["Reliability"]) / (MAX["Reliability"] - MIN["Reliability"])
  )
}

#
# Cost/Benefit model
#

Cost = function(s, w) {
  return(
    Ramp.up.time(s, w) + Dev.cost(s, w) + Dev.time(s, w)
  )
}

Benefit = function(s, w) {
  return(
    Reliability(s, w) + Dep.time(s, w) + Battery.usage(s, w) + Resp.time(s, w)
  )
}

Profit = function(s, w) {
  return(
    Benefit(s, w) - Cost(s, w)
  )
}

#
# TODO: Output must and target values?
#
