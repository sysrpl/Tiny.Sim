program Playground;

{$mode delphi}

uses
  Tiny.System, Tiny.Graphics, Tiny.Application, Tiny.Physics.Scene,
  Play.Svg, Play.SvgRender,
  { TDrawPhysics }
  Play.DrawPhysics,
  { TSvgScene }
  Play.SvgScene,
  { TSvgGrid }
  Play.SvgGrid,
  { TSvgPhysics }
  Play.SvgPhysics;

var
  Scene: TSceneClass;
  I: Integer;
begin
  while True do
  begin
    I := MessageBox('Playground - F1 toogles fullscreen',
      'Choose a from the list of scenes below:', mtInformation,
      ['Draw Physics', 'SVG Icons', 'SVG Images', 'Live Blueprint', 'Exit']);
    case I of
      0: Scene := TDrawPhysics;
      1: Scene := TSvgGrid;
      2: Scene := TSvgScene;
      3: Scene := TSvgPhysics;
    else
      Exit;
    end;
    if Scene <> nil then
    begin
      Application.VSync := False;
      Application.Fullscreen := True;
      Application.Run(Scene);
    end;
  end;
end.

