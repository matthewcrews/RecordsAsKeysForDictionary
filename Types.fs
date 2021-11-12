module Types

type BufferState =
    | Full
    | Partial
    | Empty

type Settings = {
    Levels : array<float>
    MaxRates : array<float>
    Buffers : array<BufferState>
}

printfn "%.100f" (0.1 + 0.2)
