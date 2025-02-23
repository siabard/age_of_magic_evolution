unit scene_map;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, asset_manager, sdl2, textbox, scene, KeyInput;

type


  TSceneMap = class(TScene)
  protected
    ATextBox: TTextBox;
  public
    constructor Create(AM: TAssetManager; AR: PSDL_Renderer; AK: TKeyInput);
    destructor Destroy; override;
    procedure SceneUpdate(dt: real);
    procedure SceneRender;
    procedure AnimationSystem(dt: real);
    procedure RenderSystem;

  end;

implementation

uses
  entity,
  component,
  animation,
  game_types,
  Generics.Collections;

constructor TSceneMap.Create(AM: TAssetManager; AR: PSDL_Renderer; AK: TKeyInput);
var
  AEntity: TEntity;
  AAnimation: TAnimation;
  AAnimComp: TAnimationComponent;
  APosComp: TPositionComponent;
begin
  inherited;

  FSceneType := map_scene;
  ATextBox := TTextBox.Create(0, 0, 192, 32, 4, 4);

  ATextBox.boxTexture := FAssetManager.GetTexture('panel');
  // 텍스트박스 폰트 설정
  ATextBox.korFontTexture := FAssetManager.GetTexture('hangul');
  ATextBox.engFontTexture := FAssetManager.GetTexture('ascii');

  // 신규 엔터티 생성해보기
  AEntity := FEntityManager.AddEntity();
  // animation sword 가짐
  AAnimation := FAssetManager.GetAnimation('sword');
  AAnimComp := TAnimationComponent.Create('sword anim');
  AAnimComp.SetAnimation('sword', AAnimation);
  AAnimComp.CurrentAnimation := 'sword';
  AAnimComp.Duration:=300;
  APosComp := TPositionComponent.Create('sword pos');
  APosComp.X := 120;
  APosComp.Y := 150;
  APosComp.PX := 120;
  APosComp.PY := 150;

  AEntity.position := APosComp;
  AEntity.animation := AAnimComp;

end;

destructor TSceneMap.Destroy;
begin
  ATextBox.Free;

  inherited;
end;

procedure TSceneMap.SceneUpdate(dt: real);
begin

  inherited;
  ATextBox.Text := Format('%4d', [Trunc(dt * 1000)]);
  AnimationSystem(dt);

end;

procedure TSceneMap.SceneRender();
var
  itemTexture: PSDL_Texture;
begin
  SDL_RenderClear(FRenderer);

  {
  itemTexture := FAssetManager.GetTexture('items');
  if itemTexture <> nil then
    SDL_RenderCopy(FRenderer, itemTexture, nil, nil);
  }
  ATextBox.DrawPanel(FRenderer);

  ATextBox.Draw(FRenderer);

  Self.RenderSystem;
  SDL_RenderPresent(FRenderer);

end;


procedure TSceneMap.AnimationSystem(dt: real);
var
  AEntity: TEntity;
  AEntities: specialize TList<TEntity>;
  AAnimationComponent: TAnimationComponent;
  I: integer;
begin
  { Entities 에서 animation 이 존재하는 항목만 검색 }
  AEntities := FEntityManager.GetEntities;

  for I := 0 to AEntities.Count - 1 do
  begin
    AEntity := AEntities[I];
    if AEntity.animation <> nil then
    begin
      { Animation 에 대한 처리 }
      { Animation 에 대한 프레임 변경 }
      AAnimationComponent := AEntity.animation;
      AAnimationComponent.CurrentTick :=
        AAnimationComponent.CurrentTick + Round(dt * 1000);

      if AAnimationComponent.CurrentTick >= AAnimationComponent.Duration then
      begin
        AAnimationComponent.CurrentTick :=
          AAnimationComponent.CurrentTick - AAnimationComponent.Duration;
        AAnimationComponent.CurrentFrame := AAnimationComponent.CurrentFrame + 1;
        if AAnimationComponent.CurrentFrame > AAnimationComponent.CurrentMaxFrame then
          AAnimationComponent.CurrentFrame := 0;
      end;
    end;
  end;

end;

procedure TSceneMap.RenderSystem;
var
  AEntity: TEntity;
  AEntities: specialize TList<TEntity>;
  AAnimComp: TAnimationComponent;
  APosComp: TPositionComponent;
  I: integer;
  SrcRect: TSDL_Rect;
  DstRect: TSDL_Rect;
  AAnimation: TAnimation;
  AnimRect: RRect;
  texture: PSDL_Texture;
begin
  AEntities := FEntityManager.GetEntities;

  for I := 0 to AEntities.Count - 1 do
  begin
    AEntity := AEntities[I];

    if Assigned(AEntity.animation) and Assigned(AEntity.position) then
    begin
      // 출력할 위치
      // 현재 애니메이션의 Rect 값
      // 애니메이션의 텍스쳐 값
      AAnimComp := AEntity.animation;
      APosComp := AEntity.position;
      AAnimComp.Animations.TryGetValue(AAnimComp.CurrentAnimation, AAnimation);
      if AAnimation <> nil then
      begin
        // 현재 프레임에 걸린 값을 찾는다.
        AnimRect := AAnimation.GetFrames[AAnimComp.CurrentFrame];
        SrcRect.x := AnimRect.RX;
        SrcRect.y := AnimRect.RY;
        SrcRect.w := AnimRect.RW;
        SrcRect.h := AnimRect.RH;

        DstRect.x := APosComp.X;
        DstRect.y := APosComp.Y;
        DstRect.w := AnimRect.RW;
        DstRect.h := AnimRect.RH;

        // 텍스쳐
        texture := FAssetManager.GetTexture(AAnimation.TextureName);

        SDL_RenderCopy(FRenderer, texture, @SrcRect, @DstRect);
      end;

    end;
  end;
end;

end.
