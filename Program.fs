open System
open OpenTK.Audio.OpenAL
open System.Threading
type Pulse = float32
let sampleRate = 48000f
let pitchStandard = 440f
let volume = 0.5f
let bpm = 120f
let beatsPerSecond = 60f / bpm

type Note =
    |C = 0
    |Cs= 1
    |D = 2
    |Ds= 3
    |M = 4
    |F = 5
    |Fs= 6
    |G = 7
    |Gs= 8
    |A = 9
    |As= 10
    |B = 11

let play (wave: Pulse list) =
    let waveArray = wave |> Array.ofList

    let deviceName = ALC.GetString (ALDevice.Null, AlcGetString.DefaultDeviceSpecifier)
    let device = ALC.OpenDevice deviceName
    let context = ALC.CreateContext(device, Unchecked.defaultof<int[]>)
    ALC.MakeContextCurrent context |> ignore

    let mutable alBuffer = 0
    AL.GenBuffer(&alBuffer)
    AL.BufferData(alBuffer, ALFormat.MonoFloat32Ext, waveArray, int sampleRate)
    AL.Listener(ALListenerf.Gain, 1f)
    let mutable alSource = 0
    AL.GenSource(&alSource)
    AL.Source(alSource, ALSourcef.Gain, 1f)
    AL.Source(alSource, ALSourcei.Buffer, alBuffer)
    AL.SourcePlay(alSource)
    while AL.GetSourceState alSource = ALSourceState.Playing do
        Thread.Sleep(10)
    AL.SourceStop(alSource)

    ALC.MakeContextCurrent ALContext.Null |> ignore
    ALC.DestroyContext context
    ALC.CloseDevice device |> ignore


let getWave step duration =
    [ 0f .. (sampleRate * duration) ]
    |> List.map ((*) step)
    |> List.map MathF.Sin
    |> List.map ((*) volume)

let freq hz duration =
    let step = hz * 2f * MathF.PI / sampleRate
    let output = getWave step duration
    let attack =
        [ 0 .. output.Length-1]
        |> List.map (fun x -> MathF.Min(1f, float32 x / 1000f))
    let release = List.rev attack


    List.map3 (fun a b c -> a * b * c) output attack release

let f n =
    MathF.Pow(MathF.Pow(2f, 1.0f / 12.0f), n)
    * pitchStandard

let noteFreq n beats = freq (f n) (beats * beatsPerSecond)

let note note' beats (octave: int) =
    let pos = float32 <| (int note' - int Note.A) + octave * Enum.GetNames<Note>().Length
    noteFreq pos beats

let intro =
    [ note Note.A 0.5f 0
      note Note.A 0.5f 0
      note Note.A 0.5f 0
      note Note.A 0.5f 0

      note Note.Cs 0.5f 1
      note Note.Cs 0.5f 1
      note Note.Cs 0.5f 1
      note Note.Cs 0.5f 1

      note Note.B 0.5f 0
      note Note.B 0.5f 0
      note Note.B 0.5f 0
      note Note.B 0.5f 0

      note Note.M 0.5f 1
      note Note.M 0.5f 1
      note Note.M 0.5f 1
      note Note.M 0.5f 1

      note Note.Fs 0.5f 1
      note Note.Fs 0.5f 1
      note Note.Fs 0.5f 1
      note Note.Fs 0.5f 1

      note Note.Fs 0.5f 1
      note Note.Fs 0.5f 1
      note Note.Fs 0.5f 1
      note Note.Fs 0.5f 1

      note Note.Fs 0.5f 1
      note Note.Fs 0.5f 1
      note Note.Fs 0.5f 1
      note Note.Fs 0.5f 1

      note Note.B 0.5f 0
      note Note.A 0.5f 0
      note Note.Gs 0.5f 0
      note Note.M 0.5f 0
    ] |> List.collect id

let verso =
    [ note Note.Fs 1f 0
      note Note.Fs 0.5f 0
      note Note.Cs 0.5f 1
      note Note.B 1f 0
      note Note.A 1f 0
      note Note.Gs 1f 0
      note Note.Gs 0.5f 0
      note Note.Gs 0.5f 0
      note Note.B 1f 0
      note Note.A 0.5f 0
      note Note.Gs 0.5f 0
      note Note.Fs 1f 0

      note Note.Fs 0.5f 0
      note Note.A 0.5f 1
      note Note.Gs 0.5f 1
      note Note.A 0.5f 1
      note Note.Gs 0.5f 1
      note Note.A 0.5f 1
      note Note.Fs 1f 0

      note Note.Fs 0.5f 0
      note Note.A 0.5f 1
      note Note.Gs 0.5f 1
      note Note.A 0.5f 1
      note Note.Gs 0.5f 1
      note Note.A 0.5f 1
    ] |> List.collect id

let music =
    [for _ = 0 to 2 do
      yield! intro
      yield! verso
      yield! verso]

[<EntryPoint>]
let main argv =
    play music |> ignore
    0

