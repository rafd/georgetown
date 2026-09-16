# Simulation Loop

a simulation tick runs 4 times per 'day'

epoch counts number of ticks

## Food Market

  demand based on population count
    => determine price in aggregate market

  if a citizen can't afford the price
    => go hungry (increase physical-stress and mental-stress)

  if hungry citizens...
    rerun market with new (lower) demand (and lower price)
     to identify which suppliers get comped
     (even though the price may drop, potentially bringing some of the 'hungry' citizens back in, we don't rerun again)


## Housing Market

  demand based on population count
    => determine price in aggregate market

  if a citizen can't afford the price
    => go 'unhoused' (increase physical-stress and mental-stress)

  if unhoused citizens...
    rerun market with new (lower) demand (and lower price)
     to identify which suppliers get comped
     (even though the price may drop, potentially bringing some of the 'unhoused' citizens back in, we don't rerun again)



## Work and Leisure Market

linear(?) optimization algorithm

inputs:
     citizen preferences
     citizen talents
     citizen skills
     citizen savings

     offers (ways of spending time, work and leisure)
       capacity
       time-shifts enabled
       skill-productivity-weights
       effects (from-citizen, to-citizen)


     food-price
     shelter-price

variables:
  offer choice 0..N
    per citizen

constraints:
  for each citizen:
    citizen.savings > citizen.expenses
  for each offer:
    citizens-per-offer < offer.capacity

for each tick (shift):
  or-tools, optimize for:
    sum of sqrt(citizen.joy(offer))


citizen.joy(offer):
  income
  days-of-savings (based on current food-prices, shelter-price)
  security preference

  citizen.preferences (physical, mental, etc.) X offer.skills


## Post Market Effects

citizens increase skill
   f(existing skill level, talent, work selected)

citizens increase/decrease other factors
  based on :offerable/effects

players increase in resources
  based on :offerable/effects
  based on productivity
    productivity = f(citizen.skills, offer.weights)

## Other Effects

player debt payments

regulator payments

births
  for now:
    random f(population)
      age 0
      random preference
  TODO later
    pick 2 citizens, mix of their DNA

immigration
  random f(TODO - economic slack?)
     age 20+ ish
     random preferences, skills


deaths
  per citizen, random f(age,physical-stress,mental-stress)

stress amp
   per citizen, increase physical-stress, random f(age,physical-stress)
   per citizen, increase mental-stress, random f(age,mental-stress)

skill-decline
   per citizen, skill * f(age,physical-stress,mental-stress)




