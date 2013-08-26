#
# Cost-related outcomes
#
Comp.cost = function(s, p) {
  return(
    Loc.finding("Dev.cost", s, p) + HW.platform("Dev.cost", s, p) + File.sharing("Dev.cost", s, p) + Report.sync("Dev.cost", s, p) + Chat("Dev.cost", s, p) + Map.access("Dev.cost", s, p) + Connectivity("Dev.cost", s, p) + DB("Dev.cost", s, p) + Arch.style("Dev.cost", s, p) + Data.exchange("Dev.cost", s, p)
  )
}

Ramp.up.time = function(s, p) {
  return(
    Loc.finding("Ramp.up.time", s, p) + HW.platform("Ramp.up.time", s, p) + File.sharing("Ramp.up.time", s, p) + Report.sync("Ramp.up.time", s, p) + Chat("Ramp.up.time", s, p) + Map.access("Ramp.up.time", s, p) + Connectivity("Ramp.up.time", s, p) + DB("Ramp.up.time", s, p) + Arch.style("Ramp.up.time", s, p) + Data.exchange("Ramp.up.time", s, p)
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

Delivery.time = function(s, p) {
  return(
    Ramp.up.time(s, p) + Dev.time(s, p) + Dep.time(s, p)
  )
}

Cost = function(s, p) {
  return(
    Comp.cost(s, p) + 10000 * Delivery.time(s, p)
  )
}

#
# Utility-related outcomed
#
Battery.usage = function(s, p) {
  return(
    Loc.finding("Battery.usage", s, p) + HW.platform("Battery.usage", s, p) + File.sharing("Battery.usage", s, p) + Report.sync("Battery.usage", s, p) + Chat("Battery.usage", s, p) + Map.access("Battery.usage", s, p) + Connectivity("Battery.usage", s, p) + DB("Battery.usage", s, p) + Arch.style("Battery.usage", s, p) + Data.exchange("Battery.usage", s, p)
  )
}

Battery.life = function(s, p) {
  battery.capacity = 5.18
  battery.usage = Battery.usage(s, p)
  return(
    1000 * battery.capacity / (10 * battery.usage)
  )
}

Resp.time = function(s, p) {
  return(
    Loc.finding("Resp.time", s, p) + HW.platform("Resp.time", s, p) + File.sharing("Resp.time", s, p) + Report.sync("Resp.time", s, p) + Chat("Resp.time", s, p) + Map.access("Resp.time", s, p) + Connectivity("Resp.time", s, p) + DB("Resp.time", s, p) + Arch.style("Resp.time", s, p) + Data.exchange("Resp.time", s, p)
  )
}

Reliability = function(s, p) {
  return(
    1/8 * (Loc.finding("Reliability", s, p) + File.sharing("Reliability", s, p) + Report.sync("Reliability", s, p) + Chat("Reliability", s, p) + Map.access("Reliability", s, p) + Connectivity("Reliability", s, p) + DB("Reliability", s, p) + Arch.style("Reliability", s, p))
  )
}


#
# Goals
#
goal.Delivery.time = function(s, p) {
  must = 45
  ideal = 30
  dep.time = Dep.time(s, p)
  res = (dep.time - must) / (ideal - must); res[res < 0] = 0; res[res > 1] = 1;
  return(
    list(sat=res, fail=mean(dep.time>must))
  )
}

goal.Battery.life = function(s, p) {
  must = 10
  ideal = 13
  battery.life = Battery.life(s, p)
  res = (battery.life - must) / (ideal - must); res[res < 0] = 0; res[res > 1] = 1;
  return (
    list(sat=res, fail=mean(battery.life<must))
  )
}

goal.Resp.time = function(s, p) {
  must = 2000
  ideal = 500
  resp.time = Resp.time(s, p)
  res = (resp.time - must) / (ideal - must); res[res < 0] = 0; res[res > 1] = 1;
  return(
    list(sat=res, fail=mean(resp.time>must))
  )
}

goal.Reliability = function(s, p) {
  must = 85
  ideal = 99.9
  reliability = Reliability(s, p)
  res = (reliability - must) / (ideal - must); res[res < 0] = 0; res[res > 1] = 1;
  return(
    list(sat=res, fail=mean(reliability<must))
  )
}

Efficiency = function(s, p) {
  # Maximize battery life and reliability and minimize response time.
  return(9 * goal.Battery.life(s, p)$sat + 7 * (1 - goal.Resp.time(s, p)$sat) + 3 * goal.Reliability(s, p)$sat)
}

Benefit = function(k, s, p) {
  return(
    k * Efficiency(s, p) - Cost(s, p)
  )
}

Failure.risk = function(s, p) {
  return(
    1 - (1-goal.Battery.life(s, p)$fail) * (1-goal.Resp.time(s, p)$fail) * (1-goal.Reliability(s, p)$fail)
  )
}
