unit Demo.AsteroidsGame;

{$mode delphi}

interface

uses
  Tiny.System,
  Tiny.Types,
  Tiny.Application,
  Tiny.Geometry,
  Tiny.Graphics,
  Demo.Scene;

{ TAsteroidsGame }

type
  TShape = TArrayList<TVec2>;

  PBullet = ^TBullet;
  TBullet = record
    Pos: TPointF;
    Speed: TPointF;
    Alive: Boolean;
    Time: Double;
  end;

  PRock = ^TRock;
  TRock = record
    Pos: TPointF;
    Speed: TPointF;
    Angle: Float;
    Alive: Boolean;
    Geometry: TShape;
    Size: Float;
    Dir: Float;
    Shape: Integer;
  end;

  TPlayer = record
    Pos: TPointF;
    Speed: TPointF;
    Angle: Float;
    Alive: Boolean;
    Geometry: TShape;
    Lives: Integer;
    Death: Double;
    Bullets: array[0..3] of TBullet;
  end;

  TSaucer = record
    Pos: TPointF;
    Alive: Boolean;
    Geometry: TShape;
    Size: Float;
    Dir: Integer;
    Flip: Integer;
    Alt: Float;
    Dist: Float;
    LastShot: Double;
    Bullets: array[0..3] of TBullet;
  end;

  PExplode = ^TExplode;
  TExplode = record
    Pos: TPointF;
    Seed: LongWord;
    Time: Double;
  end;

  TAsteroidsGame = class(TDemoScene)
  private
    FGamefield: TRectF;
    FScore: Integer;
    FHighScore: Integer;
    FPlayerShape: TShape;
    FPlayerRadius: Float;
    FPlayer: TPlayer;
    FRockShapes: array[0..2] of TShape;
    FRockRadius: Float;
    FRocks: TArrayList<TRock>;
    FNewRocks: TArrayList<TRock>;
    FRockGone: Double;
    FSaucerShape: TShape;
    FSaucerRadius: Float;
    FSaucer: TSaucer;
    FWave: Integer;
    FWaveTime: Double;
    FExplode: array[0..19] of TExplode;
    FExplodeIndex: Integer;
    FHyperspaceDown: Boolean;
    FFireDown: Boolean;
    FThrust: Boolean;
    FOverlay: IBitmap;
    FFont: IFont;
    FPen: IPen;
    FFirstRun: Boolean;
    procedure AddScore(Value: Integer);
    procedure Reset;
  protected
    function GetTitle: string; override;
    function GetDescription: string; override;
    function GetGlyph: string; override;
    function WantKeys: Boolean; override;
    procedure DoKeyDown(var Args: TKeyboardArgs); override;
    procedure DoKeyUp(var Args: TKeyboardArgs); override;
    procedure Load; override;
    procedure Logic(Width, Height: Integer; const Time: Double); override;
    procedure Render(Width, Height: Integer; const Time: Double); override;
  end;

implementation

function TAsteroidsGame.GetTitle: string;
begin
  Result := 'Asteroids';
end;

function TAsteroidsGame.GetDescription: string;
begin
  Result := 'A recreation of the classic vector arcade game Asteroids.';
end;

function TAsteroidsGame.GetGlyph: string;
begin
  Result := '󰑣';
end;

function TAsteroidsGame.WantKeys: Boolean;
begin
  Result := True;
end;

procedure TAsteroidsGame.DoKeyDown(var Args: TKeyboardArgs);
begin
  case Args.Key of
    keyLeft, keyRight, keyUp, keySpace, keyReturn: Args.Handled := True;
  else
    Args.Handled := False;
  end;
end;

procedure TAsteroidsGame.DoKeyUp(var Args: TKeyboardArgs);
begin
  case Args.Key of
    keyLeft, keyRight, keyUp, keySpace, keyReturn: Args.Handled := True;
  else
    Args.Handled := False;
  end;
end;

procedure TAsteroidsGame.AddScore(Value: Integer);
begin
  if (FScore < 10000) and (FScore + Value >= 10000) then
    FPlayer.Lives := FPlayer.Lives + 1
  else if (FScore < 25000) and (FScore + Value >= 25000) then
    FPlayer.Lives := FPlayer.Lives + 1
  else if (FScore < 50000) and (FScore + Value >= 50000) then
    FPlayer.Lives := FPlayer.Lives + 1
  else if (FScore < 75000) and (FScore + Value >= 75000) then
    FPlayer.Lives := FPlayer.Lives + 1
  else if (FScore < 100000) and (FScore + Value >= 100000) then
    FPlayer.Lives := FPlayer.Lives + 3;
  FScore := FScore + Value;
end;

procedure TAsteroidsGame.Reset;
var
  R: PRock;
  I: Integer;
begin
  if FPlayer.Lives = 0 then
  begin
    FWave := 0;
    FScore := 0;
    FPlayer.Pos.X := StudioWidth / 2;
    FPlayer.Pos.Y := StudioHeight / 2;
    FPlayer.Speed := NewPointF(0, 0);
    FPlayer.Angle := 0;
    FPlayer.Alive := False;
    FPlayer.Geometry.Length := FPlayerShape.Length;
    FPlayer.Lives := 0;
    FPlayer.Death := Time - 2;
    for I := Low(FPlayer.Bullets) to High(FPlayer.Bullets) do
      FPlayer.Bullets[I].Alive := False;
    for I := Low(FExplode) to High(FExplode) do
      FExplode[I].Seed := 0;
    FSaucer.Alive := False;
  end;
  Inc(FWave);
  FWaveTime := Time;
  FRocks.Clear;
  FRocks.Length := 5 + Round(Random * 2);
  for I := 0 to FRocks.Length - 1 do
  begin
    R := @FRocks.Items[I];
    repeat
      R.Pos.X := FGamefield.Left + Random * FGamefield.Width;
      R.Pos.Y := FGamefield.Top + Random * FGamefield.Height;
      R.Angle := Random * 5;
      R.Shape := Trunc(Random * 3);
      R.Size := 1;
      R.Dir := (Random - 0.5) * 0.01;
      R.Speed.X := (Random - 0.5);
      R.Speed.Y := (Random - 0.5);
    until (R.Pos.Distance(FPlayer.Pos) > 150) and (R.Speed.Distance > 0.2);
    R.Alive := True;
  end;
  FNewRocks.Clear;
end;

procedure TAsteroidsGame.Load;

  procedure AddPlayer(X, Y: Float);
  const
    Scale = 1;
  begin
    FPlayerShape.Push(NewPointF(X * Scale, Y * Scale));
  end;

  procedure AddRock(I: Integer; X, Y: Float);
  const
    Scale = 2.5;
  begin
    FRockShapes[I].Push(NewPointF(X * Scale, Y * Scale));
  end;

  procedure AddSaucer(X, Y: Float);
  const
    Scale = 2.5;
  begin
    FSaucerShape.Push(NewPointF(X * Scale, Y * Scale));
  end;

begin
  inherited Load;
  Randomize;
  if FOverlay = nil then
  begin
    FOverlay := Canvas.LoadBitmap('asteroids', '../assets/asteroid_ovl3.png');
    FFont := Canvas.LoadFont('hyperspace', '../assets/Hyperspace-Bold.ttf');
    FFont.Size := 32;
    FFont.Align := fontCenter;
    FFont.Layout := fontMiddle;
    FFont.Color := colorWhite;
    FGamefield := NewRectF(256, 54, 1425, 990);
    FPen := NewPen(colorWhite, 3);
    FPen.LineCap := capRound;
    FPen.LineJoin := joinRound;
    { Add the player outline vector }
    AddPlayer(-10, 10); AddPlayer(0, -20); AddPlayer(10, 10);
    AddPlayer(7, 5); AddPlayer(-7, 5);
    FPlayerRadius := 24;
    { Add the rock outline vectors }
    AddRock(0, -17.5, -7.5); AddRock(0, -7.5, -15); AddRock(0, 0.5, -11);
    AddRock(0, 11, -14.5); AddRock(0, 18.5, -8); AddRock(0, 10, -4);
    AddRock(0, 18.5, 4); AddRock(0, 10.5, 15); AddRock(0, -2.5, 11);
    AddRock(0, -2.5, 11); AddRock(0, -7, 16.5); AddRock(0, -17.5, 8.5);
    AddRock(0, -12.5, 0.5); AddRock(1, -16, -8); AddRock(1, -6.5, -16);
    AddRock(1, 3, -8); AddRock(1, 12.5, -16.5); AddRock(1, 22.5, -8);
    AddRock(1, 17, 0); AddRock(1, 22, 7.5); AddRock(1, 7.5, 15.5);
    AddRock(1, -6.5, 15); AddRock(1, -6.5, 15); AddRock(1, -16.5, 7);
    AddRock(2, -13.5, -7); AddRock(2, -1, -7.5); AddRock(2, -4.5, -15);
    AddRock(2, 9, -15); AddRock(2, 23.5, -7); AddRock(2, 9.5, 0);
    AddRock(2, 23.5, 9); AddRock(2, 14.5, 16); AddRock(2, 9, 12.5);
    AddRock(2, -4.5, 16.5); AddRock(2, -13.5, 4.5); FRockRadius := 65;
    { Add the saucer outline vector }
    AddSaucer(-4, 5.5); AddSaucer(-11, 1.5); AddSaucer(-4.5, -2.5);
    AddSaucer(-2, -6.5); AddSaucer(2, -6.5); AddSaucer(4.5, -2.5);
    AddSaucer(11, 1.5); AddSaucer(4, 5.5);
    FSaucerRadius := 30;
  end;
  FPlayer.Lives := 0;
  Reset;
  FFirstRun := True;
end;

procedure TAsteroidsGame.Logic(Width, Height: Integer; const Time: Double);

  { Wrap things to the gamefield }
  procedure Wrap(var Pos: TPointF);
  begin
    if Pos.X < FGamefield.Left then
      Pos.X := FGamefield.Right
    else if Pos.X > FGamefield.Right then
      Pos.X := FGamefield.Left;
    if Pos.Y < FGamefield.Top then
      Pos.Y := FGamefield.Bottom
    else if Pos.Y > FGamefield.Bottom then
      Pos.Y := FGamefield.Top;
  end;

  { Add an explosion source }
  procedure Explode(Pos: TPointF);
  begin
    FExplode[FExplodeIndex].Pos := Pos;
    FExplode[FExplodeIndex].Seed := Round(Random * 9999) + 1;
    FExplode[FExplodeIndex].Time := Time;
    FExplodeIndex := (FExplodeIndex + 1) mod High(FExplode);
  end;

  { Spawn two rocks from one rock }
  procedure RockSplit(R: PRock);
  var
    S: Float;
    N: TRock;
    I: Integer;
  begin
    S := 1 / R.Size;
    R.Alive := False;
    Explode(R.Pos);
    for I := 0 to 1 do
    begin
      N.Angle := Random * 5;
      N.Size := R.Size;
      N.Shape := Trunc(Random * 3);
      N.Dir := (Random - 0.5) * 0.01 * S;
      N.Speed.X := (Random - 0.5) * S;
      N.Speed.Y := (Random - 0.5) * S;
      N.Pos.X := R.Pos.X - (I - 0.5) * 10;
      N.Pos.Y := R.Pos.Y - (I - 0.5) * 10;
      N.Alive := True;
      FNewRocks.Push(N);
    end;
  end;

  { Check if the player intersects a rock or saucer }
  function PlayerInShape(const Player, Shape: TShape): Boolean;
  var
    A, B: TLine2;
    I, J: Integer;
  begin
    Result := False;
    for I := 0 to Player.Length do
    begin
      A.P0 := Player[I mod Player.Length];
      A.P1 := Player[(I + 1) mod Player.Length];
      for J := 0 to Shape.Length do
        begin
          B.P0 := Shape[J mod Shape.Length];
          B.P1 := Shape[(J + 1) mod Shape.Length];
          if A.Intersects(B) then
            Exit(True);
        end;
    end;
  end;

const
  DefaultLogic = 1 / 120;
  NumLives = 3;
  TurnSpeed = 0.04;
  SpeedCap = 3;
  BulletLife = 2.5;
var
  Factor: Float;
  Hyperspace, Fire, TurnLeft, TurnRight: Boolean;
  P: TPointF;
  B: PBullet;
  R: PRock;
  I, J: Integer;
begin
  { Adjust global logic step to account for our local logic step }
  Factor := LogicStep / DefaultLogic;
  { Read input from either either the keyboard or a game controller }
  Hyperspace := InputPressed(FHyperspaceDown, keyReturn, 0, 2);
  Fire := InputPressed(FFireDown, keySpace, 0, 0);
  TurnLeft := InputAxis(keyLeft, 0, 0, -0.5);
  TurnRight := InputAxis(keyRight, 0, 0, 0.5);
  FThrust := InputAxis(keyUp, 0, 2, 0) or InputAxis(keyUp, 0, 5, 0);
  { Reset the rock field if it has been destroyed }
  for I := 0 to FRocks.Length - 1 do
    if FRocks[I].Alive then
    begin
      FRockGone := Time;
      Break;
    end;
  if Time - FRockGone > 2 then
    Reset;
  { Player logic }
  with FPlayer do
  begin
    { Start a new game if enter was pressed and player has no lives }
    if (Lives < 1) and (Hyperspace) then
    begin
      Reset;
      Alive := True;
      Lives := NumLives;
      Exit;
    end;
    { Process input }
    if Alive then
    begin
      { Hyperspace }
      if Hyperspace then
      begin
        Speed := NewPointF(0, 0);
        Pos.X := FGamefield.Left + Random * (FGamefield.Width - 200) + 100;
        Pos.Y := FGamefield.Top + Random * (FGamefield.Height - 200) + 100;
      end;
      if TurnLeft xor TurnRight then
        if TurnLeft then
          Angle := Angle + TurnSpeed * Factor
        else
          Angle := Angle - TurnSpeed * Factor;
      { Add thrust }
      if FThrust then
      begin
        P := NewPointF(0, -0.01).Rotate(Angle) * Factor;
        Speed := Speed + P;
      end;
      { Fire bullets }
      if Fire then
      begin
        for I := Low(Bullets) to High(Bullets) do
        begin
          B := @Bullets[I];
          if Time - B.Time > BulletLife then
            B.Alive := False;
          if B.Alive then
            Continue;
          B.Pos := NewPointF(0, -20).Rotate(Angle) + Pos;
          B.Speed := NewPointF(0, -4).Rotate(Angle) * Factor + Speed;
          B.Alive := True;
          B.Time := Time;
          Break;
        end;
      end;
    end;
    { Cap the maximum player speed }
    if Speed.Distance > SpeedCap then
    begin
      Speed.Normalize;
      Speed := Speed * SpeedCap;
    end;
    Pos := Pos + Speed * 2 * Factor;
    { Wrap the player position }
    Wrap(Pos);
    { And generate geometry for hit testing }
    for I := 0 to Geometry.Length - 1 do
      Geometry[I] := FPlayerShape[I].Rotate(Angle) + Pos;
    { Move bullets }
    for I := Low(Bullets) to High(Bullets) do
    begin
      B := @Bullets[I];
      if Time - B.Time > BulletLife then
        B.Alive := False;
      if not B.Alive then
        Continue;
      B.Pos := B.Pos + B.Speed;
      Wrap(B.Pos);
    end;
  end;
  { Rock logic }
  for I := 0 to FRocks.Length - 1 do
  begin
    R := @FRocks.Items[I];
    if not R.Alive then
      Continue;
    R.Angle := R.Angle + R.Dir * Factor;
    R.Pos := R.Pos + R.Speed * Factor;
    { Wrap the rock position around the gamefield }
    Wrap(R.Pos);
    R.Geometry.Length := FRockShapes[R.Shape].Length;
    { Generate rock geometry for hit testing }
    for J := 0 to R.Geometry.Length - 1 do
      R.Geometry[J] := FRockShapes[R.Shape][J].Rotate(R.Angle) * R.Size + R.Pos;
  end;
  { Saucer logic }
  with FSaucer do
  begin
    { If the player is alive and enough time has passed then ... }
    if FPlayer.Alive and (Time - FWaveTime > (15 + Random * 10) / (FWave * 0.5)) and (not Alive) then
    begin
      { Mark the time and generate the saucer data }
      FWaveTime := Time;
      Alive := True;
      { Is it a big or small saucer? More waves have a higher chance of a small saucer. }
      if Random * FWave < 1.5 then
        Size := 1
      else
        Size := 0.5;
      Alt := Random * 300 + 150;
      if Random < 0.5 then
        Dir := -1
      else
        Dir := 1;
      if Random < 0.5 then
        Flip := -1
      else
        Flip := 1;
      Dist := Random * 700 + 400;
      Pos.Y := FGamefield.Top + Alt;
      if Dir < 0 then
        Pos.X := FGamefield.Right + 50
      else
        Pos.X := FGamefield.Left - 50;
      { Don't let the saucer shoot as soon as it spawns }
      LastShot := Time + 2 + Random * 3 * Size;
    end;
    { Update the saucer position }
    if Alive then
    begin
      Pos.X := Pos.X + (1 / Size) * Dir * 0.75 * Factor;
      if Dir < 0 then
      begin
        if (FGamefield.Right - 50) - Pos.X > Dist then
          if Flip > 0 then
            Pos.Y := Pos.Y + (1 / Size) * 0.75 * Factor
          else
            Pos.Y := Pos.Y - (1 / Size) * 0.75 * Factor;
        if Pos.X < FGamefield.Left - 50 then
          Alive := False;
      end
      else
      begin
        if Pos.X - (FGamefield.Left - 50) > Dist then
          if Flip > 0 then
            Pos.Y := Pos.Y + (1 / Size) * 0.75 * Factor
          else
            Pos.Y := Pos.Y - (1 / Size) * 0.75 * Factor;
        if Pos.X > FGamefield.Right + 50 then
          Alive := False
      end;
      if Pos.Y < FGamefield.Top - 50 then
        Alive := False
      else if Pos.Y > FGamefield.Bottom + 50 then
        Alive := False;
    end;
    { Generate the saucer geometry for hit testing }
    if Alive then
    begin
      Geometry.Length := FSaucerShape.Length;
      for I := 0 to Geometry.Length - 1 do
        Geometry[I] := FSaucerShape[I] * Size + Pos;
    end;
    { If both the saucer and player are alive, shoot bullets at the player occasionally }
    if Alive and (Time > LastShot) and FPlayer.Alive then
    begin
      { Add a bit of randomness to shooting intervals }
      LastShot := Time + 0.75 + Random * Size * 2;
      for J := Low(Bullets) to High(Bullets) do
      begin
        B := @Bullets[J];
        if B.Alive then
          Continue;
        B.Pos := FSaucer.Pos;
        P := FPlayer.Pos;
        { Saucer bullets aren't 100% accurate }
        P.X := P.X + (Random - 0.5) * 100;
        P.Y := P.Y + (Random - 0.5) * 100;
        B.Speed := P - Pos;
        B.Speed.Normalize;
        B.Pos := B.Pos + B.Speed * (FSaucerRadius / 3) * Size * Factor;
        B.Speed := B.Speed * (1 / Size) * 1.5;
        B.Alive := True;
        Break;
      end;
    end;
    { Move saucer bullets }
    for I := Low(Bullets) to High(Bullets) do
    begin
      B := @Bullets[I];
      if not B.Alive then
        Continue;
      B.Pos := B.Pos + B.Speed;
      { Saucer bullets expire if they move off the gamefield }
      B.Alive := FGamefield.Contains(B.Pos.X, B.Pos.Y);
    end;
  end;
  { When rocks are destroyed they spawn two smaller rocks. These smaller rocks
    are temporarily stored a new rock collection. }
  FNewRocks.Length := 0;
  { Detect rock collisions with player and bullets }
  for I := 0 to FRocks.Length - 1 do
  begin
    R := @FRocks.Items[I];
    if not R.Alive then
      Continue;
    { Test collisions with rocks and bullets }
    for J := Low(FPlayer.Bullets) to High(FPlayer.Bullets) do
    begin
      B := @FPlayer.Bullets[J];
      if not B.Alive then
        Continue;
      if (B.Pos.Distance(R.Pos) < FRockRadius * R.Size) and
        PointInPolygon(B.Pos.X, B.Pos.Y, R.Geometry) then
      begin
        B.Alive := False;
        { Scoring for destroying a rock with a bullet }
        if R.Size = 1 then
          AddScore(20)
        else if R.Size > 0.5 then
          AddScore(50)
        else
          AddScore(100);
        R.Size := R.Size - 0.34;
        R.Alive := False;
        if R.Size < 0 then
          Explode(R.Pos)
        else
          RockSplit(R);
        Break;
      end;
    end;
    { If the rock was destroyed continue }
    if not R.Alive then
      Continue;
    { Test collisions with rocks and the player }
    if FPlayer.Alive then
      if (FPlayer.Pos.Distance(R.Pos) < FRockRadius * R.Size + FPlayerRadius) and
        PlayerInShape(FPlayer.Geometry, R.Geometry) then
      begin
        { Scoring for destroying a rock with your ship }
        if R.Size = 1 then
          AddScore(20)
        else if R.Size > 0.5 then
          AddScore(50)
        else
          AddScore(100);
        R.Size := R.Size - 0.34;
        R.Alive := False;
        if R.Size < 0 then
        begin
          R.Alive := False;
          Explode(R.Pos);
        end
        else
          RockSplit(R);
        { Record the time of death, subtract a life and explode }
        FPlayer.Alive := False;
        FPlayer.Death := Time;
        FPlayer.Lives := FPlayer.Lives - 1;
        Explode(FPlayer.Pos);
        FFirstRun := False;
        Break;
      end;
  end;
  { Rock collisions are complete. Add the new rocks to the main rock collection. }
  FRocks.PushRange(FNewRocks.Items);
  { If the player has no lives then exit }
  if FPlayer.Lives < 1 then
    Exit;
  { Check for player saucer interaction }
  if FPlayer.Alive then
  begin
    { Check if player bullets hit the saucer }
    if FSaucer.Alive then
      for I := Low(FPlayer.Bullets) to High(FPlayer.Bullets) do
      begin
        B := @FPlayer.Bullets[I];
        if not B.Alive then
          Continue;
        if (B.Pos.Distance(FSaucer.Pos) < FSaucerRadius * FSaucer.Size) and
          PointInPolygon(B.Pos.X, B.Pos.Y, FSaucer.Geometry) then
        begin
          B.Alive := False;
          FSaucer.Alive := False;
          Explode(FSaucer.Pos);
          { Scoring for destroying a saucer with a bullet }
          if FSaucer.Size > 0.75 then
            AddScore(200)
          else
            AddScore(1000);
          Break;
        end;
      end;
    { Check if saucer bullets hit the player }
    for I := Low(FSaucer.Bullets) to High(FSaucer.Bullets) do
    begin
      B := @FSaucer.Bullets[I];
      if not B.Alive then
        Continue;
      if (B.Pos.Distance(FPlayer.Pos) < FPlayerRadius) and
        PointInPolygon(B.Pos.X, B.Pos.Y, FPlayer.Geometry) then
      begin
        { The player was hit by a saucer bullet }
        B.Alive := False;
        FPlayer.Alive := False;
        FPlayer.Death := Time;
        FPlayer.Lives := FPlayer.Lives - 1;
        Explode(FPlayer.Pos);
        FFirstRun := False;
        Break;
      end;
    end;
  end;
  { Again exit if the player is out of lives }
  if FPlayer.Lives < 1 then
    Exit;
  { Check if the player ran into the saucer }
  if FPlayer.Alive and FSaucer.Alive then
    if (FPlayer.Pos.Distance(FSaucer.Pos) < FSaucerRadius * FSaucer.Size + FPlayerRadius) and
      PlayerInShape(FPlayer.Geometry, FSaucer.Geometry) then
    begin
      { Record the time of death, subtract a life and explode both the saucer and your ship }
      FSaucer.Alive := False;
      FPlayer.Alive := False;
      FPlayer.Death := Time;
      FPlayer.Lives := FPlayer.Lives - 1;
      Explode(FSaucer.Pos);
      Explode(FPlayer.Pos);
      FFirstRun := False;
      { Scoring for destroying a saucer with your ship }
      if FSaucer.Size > 0.75 then
        AddScore(200)
      else
        AddScore(1000);
    end;
  { Respawn the player in a safe space }
  if (not FPlayer.Alive) and (Time - FPlayer.Death > 4) then
  begin
    FPlayer.Angle := 0;
    FPlayer.Pos := FGamefield.MidPoint;
    FPlayer.Speed := NewPointF(0, 0);
    for I := 0 to FRocks.Length - 1 do
    begin
      R := @FRocks.Items[I];
      if not R.Alive then
        Continue;
      if R.Pos.Distance(FPlayer.Pos) < 150 then
        Exit;
    end;
    FPlayer.Alive := True;
  end;
end;

procedure TAsteroidsGame.Render(Width, Height: Integer; const Time: Double);

  procedure MoveTo(X, Y, Angle: Float; Origin: TPointF);
  begin
    with NewPointF(X, Y).Rotate(Angle) + Origin do
      Canvas.MoveTo(X, Y);
  end;

  procedure LineTo(X, Y: Float; Angle: Float; Origin: TPointF);
  begin
    with NewPointF(X, Y).Rotate(Angle) + Origin do
      Canvas.LineTo(X, Y);
  end;

  procedure DrawPlayer;
  const
    DeathTime = 1;
  var
    A, B: TPointF;
    T: Float;
    I: Integer;
    C: TColorF;
  begin
    with FPlayer do if Alive then
    begin
      for I := 0 to Geometry.Length - 1 do
        with Geometry[I] do
          if I = 0 then Canvas.MoveTo(X, Y) else Canvas.LineTo(X, Y);
      Canvas.ClosePath;
      if FThrust then
      begin
        T := Sin(Time * 20) * 2.5;
        MoveTo(-5, 9, Angle, Pos);
        LineTo(-5, 13 + T, Angle, Pos);
        MoveTo(0, 8, Angle, Pos);
        LineTo(0, 15 + T, Angle, Pos);
        MoveTo(5, 9, Angle, Pos);
        LineTo(5, 13 + T, Angle, Pos);
      end;
      Canvas.Stroke(FPen);
    end
    else
    begin
      T := Time - FPlayer.Death;
      if T > DeathTime then
        Exit;
      T := T / DeathTime;
      RandSeed := Round(FPlayer.Death * 50);
      for I := 0 to FPlayerShape.Length - 2 do
      begin
        A := FPlayerShape.Items[I];
        B := FPlayerShape.Items[I + 1];
        A := A + A * T;
        B := B + B * T;
        MoveTo(A.X, A.Y, Angle + T * (Random - 0.5) * 5, Pos);
        LineTo(B.X, B.Y, Angle + T * (Random - 0.5) * 5, Pos);
      end;
      C := colorWhite;
      C.A := 1 - T;
      Canvas.Stroke(C, 2);
      Randomize;
    end;
  end;

  procedure DrawBullets;
  const
    BulletSize = 3;
  var
    B: PBullet;
    I: Integer;
  begin
    for I := Low(FPlayer.Bullets) to High(FPlayer.Bullets) do
    begin
      B := @FPlayer.Bullets[I];
      if not B.Alive then
        Continue;
      Canvas.Circle(B.Pos.X, B.Pos.Y, BulletSize);
    end;
    for I := Low(FSaucer.Bullets) to High(FSaucer.Bullets) do
    begin
      B := @FSaucer.Bullets[I];
      if not B.Alive then
        Continue;
      Canvas.Circle(B.Pos.X, B.Pos.Y, BulletSize);
    end;
    Canvas.Fill(colorWhite);
  end;

  procedure DrawSaucer;
  var
    I: Integer;
  begin
    with FSaucer do
    begin
      if not Alive then
        Exit;
      for I := 0 to Geometry.Length - 1 do
        with Geometry[I] do
          if I = 0 then Canvas.MoveTo(X, Y) else Canvas.LineTo(X, Y);
      Canvas.ClosePath;
      with Geometry[1] do Canvas.MoveTo(X, Y);
      with Geometry[6] do Canvas.LineTo(X, Y);
      with Geometry[2] do Canvas.MoveTo(X, Y);
      with Geometry[5] do Canvas.LineTo(X, Y);
      Canvas.Stroke(FPen);
    end;
  end;

  procedure DrawRocks;
  var
    R: PRock;
    P: TPointF;
    I, J, K: Integer;
  begin
    for I := 0 to FRocks.Length - 1 do
    begin
      R := @FRocks.Items[I];
      if not R.Alive then
        Continue;
      for J := 0 to 4 do
      begin
        for K := 0 to R.Geometry.Length - 1 do
        begin
          P := NewPointF(0, 0);
          case J of
            1: P.X := P.X - FGamefield.Width;
            2: P.X := P.X + FGamefield.Width;
            3: P.Y := P.Y - FGamefield.Height;
            4: P.Y := P.Y + FGamefield.Height;
          end;
          with R.Geometry[K] + P do
            if K = 0 then Canvas.MoveTo(X, Y) else Canvas.LineTo(X, Y);
        end;
        Canvas.ClosePath;
        Canvas.Stroke(FPen);
      end;
    end;
  end;

  procedure DrawExplode;
  const
    ExplodeLife = 1;
    ExplodeSize = 2;
  var
    E: PExplode;
    L: Float;
    P: TPointF;
    C: TColorF;
    I, J: Integer;
  begin
    for I := Low(FExplode) to High(FExplode) do
    begin
      E := @FExplode[I];
      if E.Seed = 0 then
        Continue;
      L := Time - E.Time;
      if L > ExplodeLife then
        Continue;
      RandSeed := E.Seed;
      for J := 0 to 19 do
      begin
        P.X := (Random - 0.5) * 150 * L;
        P.Y := (Random - 0.5) * 150 * L;
        Canvas.Circle(P.X + E.Pos.X, P.Y + E.Pos.Y, ExplodeSize);
        C := colorWhite;
        C.A := (ExplodeLife - L) / ExplodeLife;
        Canvas.Fill(C);
      end;
    end;
    Randomize;
  end;

  procedure DrawScore;
  var
    S: Float;
    P: TPointF;
    I, J: Integer;
  begin
    FFont.Align := fontLeft;
    Canvas.DrawText(FFont, IntToStr(FScore), FGamefield.Left + 200, FGamefield.Top + 50);
    Canvas.DrawText(FFont, '0', FGamefield.Right - 290, FGamefield.Top + 50);
    FFont.Align := fontCenter;
    if FScore > FHighScore then
      FHighScore := FScore;
    S := FFont.Size;
    FFont.Size := Round(FFont.Size * 0.75);
    Canvas.DrawText(FFont, IntToStr(FHighScore), FGamefield.MidPoint.X, FGamefield.Top + 25);
    FFont.Size := S;
    S := FPen.Width;
    FPen.Width := 1.5;
    for I := 0 to FPlayer.Lives - 2 do
    begin
      for J := 0 to FPlayerShape.Length - 1 do
      begin
        P := FPlayerShape[J] * 0.6;
        P.Move(FGamefield.Left + 180 - I * 20, FGamefield.Top + 55);
        if J = 0 then
          Canvas.MoveTo(P.X, P.Y)
        else
          Canvas.LineTo(P.X, P.Y)
      end;
      Canvas.ClosePath;
      Canvas.Stroke(FPen);
    end;
    FPen.Width := S;
  end;

const
  Help =
    'GAME OVER';
  Controls =
    'PRESS ENTER TO START'#13+
    'ARROW KEYS TURN AND THRUST'#10 +
    'SPACEBAR SHOOTS BULLETS'#10 +
    'ENTER GOES TO HYPERSPACE';
begin
  inherited Render(Width, Height, Time);
  ScaleToStudio;
  DrawPlayer;
  DrawBullets;
  DrawSaucer;
  DrawRocks;
  DrawExplode;
  DrawScore;
  Canvas.DrawImage(FOverlay, 0, 0);
  if FPlayer.Lives < 1 then
    with ClientRect.MidPoint do
      if FFirstRun then
      begin
        Canvas.DrawTextMemo(FFont, Help, 0, Y - 100, StudioWidth);
        Canvas.DrawTextMemo(FFont, Controls, 0, Y, StudioWidth);
      end
      else
        Canvas.DrawTextMemo(FFont, Help, 0, Y, StudioWidth);
end;

end.

