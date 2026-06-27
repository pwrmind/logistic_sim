namespace SortingCenterSimulation

open System

// ============================================================================
// МОДУЛЬ CORE – ДОМЕННАЯ МОДЕЛЬ (Wlaschin + Meadows)
// ============================================================================
module Core =

    // === Асинхронный Result (исправленный билдер + функции map/mapError/bind) ===
    type AsyncResult<'T, 'E> = Async<Result<'T, 'E>>

    module AsyncResult =
        let ofResult (r: Result<'T, 'E>) : AsyncResult<'T, 'E> = async { return r }

        let map (f: 'T -> 'U) (ar: AsyncResult<'T, 'E>) : AsyncResult<'U, 'E> =
            async {
                let! r = ar
                return Result.map f r
            }

        let mapError (f: 'E -> 'F) (ar: AsyncResult<'T, 'E>) : AsyncResult<'T, 'F> =
            async {
                let! r = ar
                return Result.mapError f r
            }

        let bind (f: 'T -> AsyncResult<'U, 'E>) (ar: AsyncResult<'T, 'E>) : AsyncResult<'U, 'E> =
            async {
                let! r = ar
                match r with
                | Ok x -> return! f x
                | Error e -> return Error e
            }

    type AsyncResultBuilder() =
        member _.Bind(m: AsyncResult<'T, 'E>, f: 'T -> AsyncResult<'U, 'E>) : AsyncResult<'U, 'E> =
            AsyncResult.bind f m
        member _.Return(x: 'T) : AsyncResult<'T, 'E> = async { return Ok x }
        member _.ReturnFrom(m: AsyncResult<'T, 'E>) = m
        member _.Zero() : AsyncResult<unit, 'E> = async { return Ok () }
        member _.Combine(a: AsyncResult<unit, 'E>, b: AsyncResult<'T, 'E>) : AsyncResult<'T, 'E> =
            async {
                let! ra = a
                match ra with
                | Ok () -> return! b
                | Error e -> return Error e
            }
        member _.Delay(f: unit -> AsyncResult<'T, 'E>) : AsyncResult<'T, 'E> = async { return! f() }
        member _.Run(f: AsyncResult<'T, 'E>) = f

    let asyncResult = AsyncResultBuilder()

    // === Базовые constrained типы ===
    type PackageId = private PackageId of string
    module PackageId =
        let create (s: string) =
            if String.IsNullOrWhiteSpace s then Error "PackageId cannot be empty"
            else Ok (PackageId s)
        let value (PackageId id) = id

    type ContainerId = private ContainerId of string
    module ContainerId =
        let create (s: string) =
            if String.IsNullOrWhiteSpace s then Error "ContainerId cannot be empty"
            else Ok (ContainerId s)
        let value (ContainerId id) = id

    type StationId = private StationId of string
    module StationId =
        let create (s: string) = Ok (StationId s)
        let value (StationId id) = id

    type WorkerId = private WorkerId of string
    type ConveyorId = private ConveyorId of string

    type WeightKg = private WeightKg of decimal
    module WeightKg =
        let create w =
            if w <= 0m then Error "Weight must be positive"
            elif w > 100m then Error "Weight exceeds max 100kg"
            else Ok (WeightKg w)

    type Dimensions = {
        LengthCm: decimal
        WidthCm:  decimal
        HeightCm: decimal
    }

    type ZoneCode = private ZoneCode of string
    module ZoneCode =
        let create (s: string) =
            if s.Length >= 2 && Char.IsLetter s.[0] then Ok (ZoneCode (s.ToUpper()))
            else Error "Invalid zone code format"
        let value (ZoneCode z) = z

    type Priority =
        | Standard
        | Express
        | Fragile

    type SortingCategory =
        | ByDestination of ZoneCode
        | ByCarrier     of string
        | ByPriority    of Priority

    type ContainerCapacity = private ContainerCapacity of int
    module ContainerCapacity =
        let create n =
            if n <= 0 then Error "Capacity must be positive"
            elif n > 500 then Error "Max container capacity is 500"
            else Ok (ContainerCapacity n)

    // === Состояния посылки (State Machine) ===
    type UnregisteredPackage = {
        TrackingNumber: string
        RawWeight:      decimal
        RawDimensions:  Dimensions
        RawDestination: string
    }

    type ReceivedPackage = {
        PackageId:   PackageId
        Weight:      WeightKg
        Dimensions:  Dimensions
        Destination: ZoneCode
        Priority:    Priority
        ReceivedAt:  DateTimeOffset
    }

    type SortedPackage = {
        PackageId:       PackageId
        Weight:          WeightKg
        Destination:     ZoneCode
        Priority:        Priority
        SortingCategory: SortingCategory
        AssignedStation: StationId
        SortedAt:        DateTimeOffset
    }

    type StagedPackage = {
        PackageId:       PackageId
        ContainerId:     ContainerId
        SortingCategory: SortingCategory
        StagedAt:        DateTimeOffset
    }

    type DispatchedPackage = {
        PackageId:     PackageId
        ContainerId:   ContainerId
        DispatchedAt:  DateTimeOffset
    }

    // === Системные сущности (Stocks) ===
    type StationStatus = Idle | Processing | Maintenance | Blocked

    type SortingStation = {
        StationId:      StationId
        Category:       SortingCategory
        ProcessingRate: decimal
        CurrentStatus:  StationStatus
        CurrentLoad:    int
        MaxCapacity:    int
    }

    type ContainerStatus = Empty | Filling | Full | InTransit

    type Container = {
        ContainerId:  ContainerId
        TargetZone:   ZoneCode
        Status:       ContainerStatus
        PackageCount: int
        MaxCapacity:  ContainerCapacity
    }

    type ConveyorBelt = {
        ConveyorId:        ConveyorId
        FromStation:       StationId option
        ToStation:         StationId option
        SpeedItemsPerMin:  decimal
        CurrentItems:      int
        MaxItems:          int
        TransportDelay:    TimeSpan
    }

    // === Команды и События ===
    type SortingCenterCommand =
        | ReceivePackage      of UnregisteredPackage
        | AssignToStation     of PackageId * StationId
        | SortPackage         of PackageId
        | StageToContainer    of PackageId * ContainerId
        | DispatchContainer   of ContainerId
        | ReallocateWorkers   of WorkerId list * StationId
        | ScheduleMaintenance of StationId * DateTimeOffset
        | RecalculateMetrics  of DateTimeOffset

    type SortingCenterEvent =
        | PackageReceived      of ReceivedPackage
        | PackageRejected      of PackageId: string * Reason: string
        | PackageAssigned      of PackageId * StationId
        | PackageSorted        of SortedPackage
        | StationOverloaded    of StationId * CurrentLoad: int * MaxCapacity: int
        | PackageStaged        of StagedPackage
        | ContainerFilled      of ContainerId * ZoneCode
        | ContainerDispatched  of ContainerId * DispatchedAt: DateTimeOffset
        | WorkersReallocated   of WorkerId list * StationId
        | StationStatusChanged of StationId * StationStatus
        | BottleneckDetected   of StationId * QueueSize: int * Threshold: int
        | ThroughputMeasured   of Period: TimeSpan * PackagesProcessed: int
        | DelayExceeded        of PackageId * ExpectedTime: TimeSpan * ActualTime: TimeSpan

    // === Типы ошибок ===
    type ReceiveError =
        | ValidationError of string
        | DuplicatePackage of PackageId

    type SortError =
        | StationNotFound of string

    type DispatchError =
        | ContainerNotFull of ContainerId

    // === Зависимости (внешние порты) ===
    type CheckDuplicate     = PackageId -> AsyncResult<bool, string>
    type RegisterPackage    = ReceivedPackage -> AsyncResult<unit, string>
    type PublishEvent       = SortingCenterEvent -> AsyncResult<unit, string>
    type FindOptimalStation = SortingCategory -> StationId list -> AsyncResult<StationId, string>
    type UpdateStationLoad  = StationId -> int -> AsyncResult<unit, string>
    type CheckContainerFull = ContainerId -> AsyncResult<bool, string>
    type LoadToVehicle      = ContainerId -> AsyncResult<unit, string>

    // --- Пайплайн "Приёмка" (исправлено) ---
    let receivePackageWorkflow
        (checkDuplicate: CheckDuplicate)
        (registerPackage: RegisterPackage)
        (publishEvent: PublishEvent)
        (command: UnregisteredPackage)
        : AsyncResult<SortingCenterEvent, ReceiveError> =
        asyncResult {
            let! packageId =
                command.TrackingNumber
                |> PackageId.create
                |> Result.mapError ValidationError
                |> AsyncResult.ofResult
            let! weight =
                command.RawWeight
                |> WeightKg.create
                |> Result.mapError ValidationError
                |> AsyncResult.ofResult
            let! dest =
                command.RawDestination
                |> ZoneCode.create
                |> Result.mapError ValidationError
                |> AsyncResult.ofResult

            let! isDuplicate = checkDuplicate packageId |> AsyncResult.mapError ValidationError
            if isDuplicate then
                return! Error (DuplicatePackage packageId) |> AsyncResult.ofResult
            else
                let received = {
                    PackageId   = packageId
                    Weight      = weight
                    Dimensions  = command.RawDimensions
                    Destination = dest
                    Priority    = Standard
                    ReceivedAt  = DateTimeOffset.UtcNow
                }
                do! registerPackage received |> AsyncResult.mapError ValidationError
                let event = PackageReceived received
                do! publishEvent event |> AsyncResult.mapError ValidationError
                return event
        }

    // --- Пайплайн "Сортировка" ---
    let sortPackageWorkflow
        (findOptimalStation: FindOptimalStation)
        (updateStationLoad: UpdateStationLoad)
        (publishEvent: PublishEvent)
        (package: ReceivedPackage)
        : AsyncResult<SortingCenterEvent, SortError> =
        asyncResult {
            let category = ByDestination package.Destination
            let! stationId =
                findOptimalStation category []
                |> AsyncResult.mapError (fun msg -> StationNotFound msg)
            do! updateStationLoad stationId 1 |> AsyncResult.mapError (fun msg -> StationNotFound msg)

            let sorted = {
                PackageId       = package.PackageId
                Weight          = package.Weight
                Destination     = package.Destination
                Priority        = package.Priority
                SortingCategory = category
                AssignedStation = stationId
                SortedAt        = DateTimeOffset.UtcNow
            }
            let event = PackageSorted sorted
            do! publishEvent event |> AsyncResult.mapError (fun msg -> StationNotFound msg)
            return event
        }

    // --- Пайплайн "Отгрузка" (исправлено) ---
    let dispatchContainerWorkflow
        (checkFull: CheckContainerFull)
        (loadToVehicle: LoadToVehicle)
        (publishEvent: PublishEvent)
        (containerId: ContainerId)
        : AsyncResult<SortingCenterEvent, DispatchError> =
        asyncResult {
            let! isFull = checkFull containerId |> AsyncResult.mapError (fun _ -> ContainerNotFull containerId)
            if not isFull then
                return! Error (ContainerNotFull containerId) |> AsyncResult.ofResult
            else
                do! loadToVehicle containerId |> AsyncResult.mapError (fun _ -> ContainerNotFull containerId)
                let dispatchedAt = DateTimeOffset.UtcNow
                let event = ContainerDispatched(containerId, dispatchedAt)
                do! publishEvent event |> AsyncResult.mapError (fun _ -> ContainerNotFull containerId)
                return event
        }

    // === Системное мышление: обратная связь и узкие места ===
    type BottleneckThreshold = private BottleneckThreshold of int
    module BottleneckThreshold =
        let create n =
            if n <= 0 then Error "Threshold must be positive"
            else Ok (BottleneckThreshold n)

    type MonitorMetrics = {
        StationId:      StationId
        QueueSize:      int
        ProcessingRate: decimal
        ArrivalRate:    decimal
        AvgWaitTime:    TimeSpan
        Utilization:    decimal
    }

    let detectBottleneck
        (threshold: BottleneckThreshold)
        (metrics: MonitorMetrics)
        : SortingCenterEvent option =
        let (BottleneckThreshold t) = threshold
        if metrics.QueueSize > t then
            Some (BottleneckDetected(metrics.StationId, metrics.QueueSize, t))
        elif metrics.ArrivalRate > metrics.ProcessingRate * 1.2m then
            Some (BottleneckDetected(metrics.StationId, metrics.QueueSize, t))
        else
            None

    type ReallocateStrategy =
        | Proportional
        | Aggressive
        | Conservative

    let calculateReallocation
        (strategy: ReallocateStrategy)
        (sourceMetrics: MonitorMetrics)
        (targetMetrics: MonitorMetrics)
        (availableWorkers: WorkerId list)
        : WorkerId list =
        match strategy with
        | Conservative ->
            availableWorkers |> List.tryHead |> Option.toList
        | Proportional ->
            let overloadRatio =
                if targetMetrics.ProcessingRate > 0m then
                    targetMetrics.ArrivalRate / targetMetrics.ProcessingRate
                else 1.0m
            let needed = int (ceil (overloadRatio - 1.0m) * decimal sourceMetrics.QueueSize)
            let count = min needed (List.length availableWorkers)
            availableWorkers |> List.take count
        | Aggressive ->
            availableWorkers

    type CoreSystemState = {
        Timestamp:     DateTimeOffset
        TotalInQueue:  int
        Throughput:    int
        ActiveWorkers: int
    }

    type OscillationDetector = {
        History:    CoreSystemState list
        WindowSize: int
    }

    let detectOscillation (detector: OscillationDetector) : bool =
        if detector.History.Length < detector.WindowSize then false
        else
            let recent = detector.History |> List.take detector.WindowSize
            let throughputs = recent |> List.map (fun s -> s.Throughput)
            let changes =
                throughputs
                |> List.pairwise
                |> List.map (fun (a, b) -> sign (b - a))
            let alternations =
                changes
                |> List.pairwise
                |> List.filter (fun (a, b) -> a <> b && a <> 0 && b <> 0)
            let totalNonZero = changes |> List.filter ((<>) 0) |> List.length
            if totalNonZero = 0 then false
            else float (List.length alternations) / float totalNonZero > 0.6

// ============================================================================
// МОДУЛЬ SIMULATION – РАБОЧАЯ ИМИТАЦИЯ (Stocks & Flows)
// ============================================================================
module Simulation =

    type StationId = string

    type StationState = {
        Id: StationId
        BaseCapacityPerWorker: int
        Workers: int
        Queue: int
        ProcessedTotal: int
    }

    type SystemState = {
        Tick: int
        TotalReceived: int
        TotalDispatched: int
        Stations: Map<StationId, StationState>
        History: int list
        EventsLog: string list
    }

    type SimParams = {
        PerceptionDelay: int
        ResponseDelay: int
        BottleneckThreshold: int
        MaxTotalWorkers: int
    }

    let generateInflow (tick: int) (rng: Random) =
        if tick > 20 && tick < 40 then rng.Next(30, 50)
        else rng.Next(5, 15)

    let processStation (station: StationState) =
        let capacity = station.Workers * station.BaseCapacityPerWorker
        let processed = min station.Queue capacity
        { station with
            Queue = station.Queue - processed
            ProcessedTotal = station.ProcessedTotal + processed
        }, processed

    let detectBottlenecks (state: SystemState) (p: SimParams) =
        state.Stations
        |> Map.toList
        |> List.choose (fun (id, st) ->
            if st.Queue > p.BottleneckThreshold then
                Some (sprintf "🚨 BOTTLENECK at %s: Queue=%d (Threshold=%d)" id st.Queue p.BottleneckThreshold)
            else None)

    let getPerceivedQueue (history: int list) (delay: int) =
        if history.Length <= delay then
            history |> List.map float |> List.average |> int
        else history.[delay]

    /// Перераспределение работников при обнаружении узкого места
    let reallocateWorkers (state: SystemState) (p: SimParams) =
        let stations = state.Stations |> Map.toList
        let overloaded = stations |> List.filter (fun (_, s) -> s.Queue > p.BottleneckThreshold)
        let underloaded = stations |> List.filter (fun (_, s) ->
            s.Queue < p.BottleneckThreshold / 2 && s.Workers > 1)
        let workersToMove = if overloaded.Length > 0 && underloaded.Length > 0 then 1 else 0
        if workersToMove > 0 then
            let (targetId, _) = overloaded.Head
            let (sourceId, _) = underloaded.Head
            let update id delta =
                Map.change id (Option.map (fun s -> { s with Workers = s.Workers + delta }))
            let newStations =
                state.Stations |> update sourceId -1 |> update targetId 1
            let msg = sprintf "⚙️  REALLOCATION: Moved 1 worker from %s to %s" sourceId targetId
            newStations, [msg]
        else
            state.Stations, []

    /// Один шаг симуляции
    let simulationTick (p: SimParams) (rng: Random) (state: SystemState) =
        let tick = state.Tick + 1
        let inflow = generateInflow tick rng
        let totalReceived = state.TotalReceived + inflow

        // Входящий поток направляется на станцию A
        let stationsWithInflow =
            state.Stations |> Map.map (fun id s ->
                if id = "Station-A" then { s with Queue = s.Queue + inflow }
                else s)

        // Обработка
        let processedList, processedCounts =
            stationsWithInflow
            |> Map.toList
            |> List.map (fun (_, s) -> processStation s)
            |> List.unzip

        let totalProcessed = processedCounts |> List.sum
        let processedStations = processedList |> List.map (fun s -> s.Id, s) |> Map.ofList
        let totalDispatched = state.TotalDispatched + totalProcessed

        // Обнаружение узких мест
        let bottleneckEvents =
            detectBottlenecks { state with Stations = processedStations } p

        // Возврат работников, если нагрузка спала (простая эвристика)
        let stationsAfterReturn =
            processedStations |> Map.map (fun id s ->
                let stA = processedStations.["Station-A"]
                if id = "Station-A" && stA.Queue < p.BottleneckThreshold / 2 && s.Workers > 7 then
                    { s with Workers = s.Workers - 1 }
                elif id = "Station-B" && stA.Queue < p.BottleneckThreshold / 2 && s.Workers < 3 then
                    { s with Workers = s.Workers + 1 }
                else s
            )

        // Перераспределение при узком месте
        let finalStations, reallocationEvents =
            reallocateWorkers { state with Stations = stationsAfterReturn } p

        let allEvents = bottleneckEvents @ reallocationEvents

        let maxQueue = finalStations |> Map.toList |> List.map (fun (_, s) -> s.Queue) |> List.max
        let newHistory = maxQueue :: state.History |> List.truncate 50

        {
            Tick = tick
            TotalReceived = totalReceived
            TotalDispatched = totalDispatched
            Stations = finalStations
            History = newHistory
            EventsLog = allEvents
        }

    let printDashboard (state: SystemState) =
        let stA = state.Stations["Station-A"]
        let stB = state.Stations["Station-B"]
        let drawBar queue maxLen =
            let bars = String.replicate (min maxLen (queue / 2)) "█"
            sprintf "[%s] %d" bars queue
        let estWaitA = if stA.Workers > 0 then float stA.Queue / float (stA.Workers * stA.BaseCapacityPerWorker) else 0.0
        let estWaitB = if stB.Workers > 0 then float stB.Queue / float (stB.Workers * stB.BaseCapacityPerWorker) else 0.0

        printfn "-----------------------------------------------------------------------------------------"
        printfn "Tick: %03d | Received: %3d | Dispatched: %3d | Queue: %d | Est.wait A: %.1f B: %.1f"
            state.Tick state.TotalReceived state.TotalDispatched (stA.Queue + stB.Queue) estWaitA estWaitB
        printfn "  Station A (Main) | Workers: %d | Queue: %s" stA.Workers (drawBar stA.Queue 20)
        printfn "  Station B (Help) | Workers: %d | Queue: %s" stB.Workers (drawBar stB.Queue 20)
        if state.EventsLog.Length > 0 then
            state.EventsLog |> List.iter (fun e -> printfn "  >> %s" e)
        printfn ""

    let run () =
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

        // Итоговая сводка
        let stA = currentState.Stations["Station-A"]
        let stB = currentState.Stations["Station-B"]
        printfn "\n=== ИТОГИ СИМУЛЯЦИИ ==="
        printfn "Всего поступило: %d" currentState.TotalReceived
        printfn "Всего обработано: %d" currentState.TotalDispatched
        printfn "Осталось в очередях: %d" (stA.Queue + stB.Queue)
        printfn "Пиковая очередь (за всю историю): %d" (currentState.History |> List.max)
        printfn "Финальное распределение работников: A=%d, B=%d" stA.Workers stB.Workers

// ============================================================================
// ТОЧКА ВХОДА
// ============================================================================
module Program =
    [<EntryPoint>]
    let main _ =
        Simulation.run()
        0