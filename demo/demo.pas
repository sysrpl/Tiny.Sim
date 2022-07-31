program Demo;

{$mode delphi}

uses
  { This brings in out tiny library system }
  Tiny.Application,
  Tiny.Widgets,
  Tiny.Widgets.Custom,
  Tiny.Widgets.Themes,
  Tiny.Text.Json,
  Tiny.Text.Glyphs,
  { Our program units }
  Demo.Viewer,
  Demo.Scene,
  Demo.AsteroidsGame,
  Demo.AtomicDots,
  Demo.CrowdWalk,
  Demo.FireSparks,
  Demo.TextBrush,
  Demo.LightBeams,
  Demo.MouseTrack,
  Demo.VectorClock,
  Demo.Synthwave;

{$R *.res}

begin
  Application.VSync := True;
  Application.Fullscreen := True;
  Application.Run(TDemoViewer);
end.

