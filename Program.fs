module SortingCenterSimulation

open System

// ============================================================================
// ДОМЕННЫЕ ТИПЫ
// ============================================================================

/// Идентификатор станции
type StationId = string

/// Состояние одной сортировочной станции
type StationState = {
    Id: StationId
    BaseCapacityPerWorker: int   // Производительность одного работника за такт
    Workers: int                 // Количество работников на станции
    Queue: int                   // Текущая очередь посылок (запас)
    ProcessedTotal: int          // Всего обработано (исходящий поток)
}

/// Глобальное состояние системы
type SystemState = {
    Tick: int                    // Номер такта
    TotalReceived: int           // Всего поступило посылок
    TotalDispatched: int         // Всего отправлено (обработано)
    Stations: Map<StationId, StationState>
    History: int list            // История максимальной очереди (для задержек восприятия)
    EventsLog: string list       // События текущего такта
}

/// Параметры симуляции (точки воздействия)
type SimulationParams = {
    PerceptionDelay: int         // Задержка восприятия (тактов)
    ResponseDelay: int           // Задержка реакции (не используется явно, но влияет на политику)
    BottleneckThreshold: int     // Порог очереди для срабатывания узкого места
    MaxTotalWorkers: int         // Общий лимит работников
}

// ============================================================================
// ПОТОКИ И ЗАПАСЫ
// ============================================================================

/// Генератор входящего потока с имитацией пиковой нагрузки
let generateInflow (tick: int) (rng: Random) =
    if tick > 20 && tick < 40 then rng.Next(30, 50)  // высокая нагрузка
    else rng.Next(5, 15)                             // обычная нагрузка

/// Обработка очереди на одной станции (исходящий поток)
let processStation (station: StationState) =
    let capacity = station.Workers * station.BaseCapacityPerWorker
    let processed = min station.Queue capacity
    { station with
        Queue = station.Queue - processed
        ProcessedTotal = station.ProcessedTotal + processed
    }, processed

// ============================================================================
// ОБРАТНАЯ СВЯЗЬ И ЗАДЕРЖКИ
// ============================================================================

/// Обнаружение узких мест (балансирующая обратная связь)
let detectBottlenecks (state: SystemState) (simParams: SimulationParams) =
    state.Stations
    |> Map.toList
    |> List.choose (fun (id, station) ->
        if station.Queue > simParams.BottleneckThreshold then
            Some (sprintf "🚨 BOTTLENECK at %s: Queue=%d (Threshold=%d)" id station.Queue simParams.BottleneckThreshold)
        else None
    )

/// Оценка воспринимаемой очереди с учётом PerceptionDelay
let getPerceivedQueue (history: int list) (delay: int) =
    if history.Length <= delay then
        history |> List.map float |> List.average |> int
    else
        history.[delay]

/// Перераспределение работников (реакция на обратную связь)
let reallocateWorkers (state: SystemState) (simParams: SimulationParams) =
    let stations = state.Stations |> Map.toList

    let overloaded = stations |> List.filter (fun (_, s) -> s.Queue > simParams.BottleneckThreshold)
    let underloaded = stations |> List.filter (fun (_, s) ->
        s.Queue < simParams.BottleneckThreshold / 2 && s.Workers > 1
    )

    let workersToMove = if overloaded.Length > 0 && underloaded.Length > 0 then 1 else 0

    if workersToMove > 0 then
        let (targetId, _) = overloaded.Head
        let (sourceId, _) = underloaded.Head

        let updateStation id delta =
            Map.change id (fun opt ->
                opt |> Option.map (fun s -> { s with Workers = s.Workers + delta })
            )

        let newStations =
            state.Stations
            |> updateStation sourceId -1
            |> updateStation targetId 1

        let logMsg = sprintf "⚙️  REALLOCATION: Moved 1 worker from %s to %s" sourceId targetId
        newStations, [logMsg]
    else
        state.Stations, []

// ============================================================================
// ШАГ СИМУЛЯЦИИ
// ============================================================================

let simulationTick (simParams: SimulationParams) (rng: Random) (state: SystemState) =
    let tick = state.Tick + 1

    // 1. Входящий поток
    let inflow = generateInflow tick rng
    let totalReceived = state.TotalReceived + inflow

    // Распределение: всё идёт на первую станцию (для наглядности)
    let stationsWithInflow =
        state.Stations |> Map.map (fun id s ->
            if id = "Station-A" then { s with Queue = s.Queue + inflow }
            else s
        )

    // 2. Обработка на станциях
    let processedList, totalProcessed =
        stationsWithInflow
        |> Map.toList
        |> List.map (fun (_, s) -> processStation s)
        |> List.unzip

    let processedStations =
        processedList |> List.map (fun s -> s.Id, s) |> Map.ofList

    let totalDispatched = state.TotalDispatched + totalProcessed.Length

    // 3. Обнаружение узких мест
    let bottleneckEvents =
        detectBottlenecks { state with Stations = processedStations } simParams

    // 4. Перераспределение работников
    let finalStations, reallocationEvents =
        reallocateWorkers { state with Stations = processedStations } simParams

    let allEvents = bottleneckEvents @ reallocationEvents

    // 5. История максимальной очереди (для задержки восприятия)
    let maxQueue = finalStations |> Map.toList |> List.map (fun (_, s) -> s.Queue) |> List.max
    let newHistory = maxQueue :: state.History |> List.truncate 50

    // 6. Новое состояние
    {
        Tick = tick
        TotalReceived = totalReceived
        TotalDispatched = totalDispatched
        Stations = finalStations
        History = newHistory
        EventsLog = allEvents
    }

// ============================================================================
// ВИЗУАЛИЗАЦИЯ
// ============================================================================

let printDashboard (state: SystemState) =
    let stA = state.Stations["Station-A"]
    let stB = state.Stations["Station-B"]

    let drawBar queue maxLen =
        let bars = String.replicate (min maxLen (queue / 2)) "█"
        sprintf "%-20s" (sprintf "[%s] %d" bars queue)

    printfn "-----------------------------------------------------------------------------------------"
    printfn "Tick: %03d | Total Received: %3d | Total Dispatched: %3d | System Queue: %d"
        state.Tick
        state.TotalReceived
        state.TotalDispatched
        (stA.Queue + stB.Queue)

    printfn "  Station A (Main) | Workers: %d | Queue: %s" stA.Workers (drawBar stA.Queue 20)
    printfn "  Station B (Help) | Workers: %d | Queue: %s" stB.Workers (drawBar stB.Queue 20)

    if state.EventsLog.Length > 0 then
        state.EventsLog |> List.iter (fun e -> printfn "  >> %s" e)
    printfn ""

// ============================================================================
// ЗАПУСК
// ============================================================================

let runSimulation () =
    let rng = Random(42)

    let simParams = {
        PerceptionDelay = 3
        ResponseDelay = 2
        BottleneckThreshold = 30
        MaxTotalWorkers = 10
    }

    let initialState = {
        Tick = 0
        TotalReceived = 0
        TotalDispatched = 0
        Stations = Map.ofList [
            "Station-A", { Id = "Station-A"; BaseCapacityPerWorker = 3; Workers = 7; Queue = 0; ProcessedTotal = 0 }
            "Station-B", { Id = "Station-B"; BaseCapacityPerWorker = 3; Workers = 3; Queue = 0; ProcessedTotal = 0 }
        ]
        History = [0]
        EventsLog = []
    }

    printfn "🏭 ЗАПУСК СИМУЛЯЦИИ СОРТИРОВОЧНОГО ЦЕНТРА"
    printfn "📖 Демонстрация: Задержки (Delays) и Колебания (Oscillations) в системах с обратной связью.\n"

    let mutable currentState = initialState

    for _ in 1..60 do
        currentState <- simulationTick simParams rng currentState
        printDashboard currentState
        System.Threading.Thread.Sleep(150)
    0

[<EntryPoint>]
let main _ =
    runSimulation()