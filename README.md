# Georgetown

Georgetown is a WIP browser-based multiplayer persistent online town building game.

The primary motivation is to see how different policies (ex. [land value taxes](https://en.wikipedia.org/wiki/Land_value_tax) and [universal basic income](https://en.wikipedia.org/wiki/Universal_basic_income)) affect player motivations and game play.

[Play it here](https://georgetown.sunnypursuits.com).

Tell me what to fix or build next [on Discord](https://discord.gg/FdPus82t4b).

## The Game

The goal of the game is grow a thriving town. It's a puzzle of making the best use of the available resources - economics!

It is not about amassing the most cash (try it - it will lead to an economics collapse of the island; not on purpose, but as of yet, cash is in fixed supply).

- Islands
  - there are multiple islands
  - each island is independent, no transfer of resources
  - a player can play on multiple islands
  - multiple players can play on an island

- Lots
  - an island has lots
  - lots can be owned or unowned
  - a player can purchase a unowned lot (by paying an ongoing tax, whose rate is self-decided)
  - a player can purchase an owned lot, by paying a higher tax than the current owner (plus the cost of existing improvements on that lot, which go to the current owner)

- Improvements
  - an improvement can be built on a lot, for a given cost
  - current available improvements:
    - house
      - offers 2 shelter for a player-decided price
    - farm
      - offers 50 food for a player-decided price
      - requires 100 labour for a player-decided price
  - improvements can be demolished, but only half the cost is recovered
  - some offers are prequisites (ex. jobs at a big-farm), if they are partially filled, then the production of the other offers is reduced (ex. only 50% of jobs on a big-farm, thus, only produce 50% of the food)
  - if a player cannot afford to pay their money supply offers (ex. jobs at big-farm) then all their offers are scaled pro-rata to what they can afford

- The Simulation
  - the game ticks 1/sec, 1 sec real world ~= 1 day in game
  - Sims
    - an island has a population of citizens ("sims")
    - sims require
       - 1 shelter per tick
       - 21 food per tick
    - government redistributes its tax revenues to sims (as a citizen's dividend)
    - sims have savings
    - sims seek food and shelter from markets
    - sim will work to meet their needs for food and shelter (with savings to cover future needs)
  - Population
    - sims have a fixed chance of dying
    - new sims arrive if there is capacity (and faster if there is more time spent on leisure)
    - sims will leave if there is insufficient capacity
    (food or shelter)
  - Markets
    - there are markets for food, shelter, and labour
    - players create offers inside of their improvements (ex. 10 food for 10 dollars)
    - based on available offers, and demand for a resource, the "best" offers will be accepted (cheapest offers for food and shelter, best paying offers for labour)
    - offers can be partially filled (ex. offer for 10 for 10 dollars, but market only purchased 5 food for 5 dollars)

## Future Plans

- most notably, the ability to have islands with different policies

## Development

- stack
  - Front End
    - [Clojurescript](https://clojure.org)
    - [Reagent](https://clojure.org)
  - Back End
     - [Clojure](https://clojure.org)
     - [Datalevin](https://github.com/juji-io/datalevin) (datomic-like graph db)
     - [Tada](https://github.com/rafd/tada)
     - Omni (unreleased SPA web framework)

- the core loop
  - front-end ajax long-polls for a single island's state
  - on database changes, all connected clients are sent new data (public and private)
  - back-end ticks every second, calculating new state (sending updates)
  - front-end user actions send an ajax request (changing database, triggering updates)

- getting started
  - install java, clojure
  - clone the repo
  - create a `config.edn`
    - ```
      {:http-port 9365
       :environment :dev
       :db-dir "data/datalevin"
       :website-base-url "http://localhost:9365"
       :auth-cookie-secret "0123456789123456"
       :auth-token-secret "0123456789123456"}
       ```
  - `lein repl`
  - `(start!)`
