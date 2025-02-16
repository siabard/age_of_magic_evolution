unit asset_manager;

{$mode ObjFPC}{$H+}

interface


uses
  Classes, SysUtils, sdl2, sdl2_image,
  Generics.Collections, LogUtil, animation;

type

  TTextureDictionary = specialize TDictionary<string, PSDL_Texture>;

  TAssetManager = class
  private
    ASdlRenderer: PSDL_Renderer;
    ATextures: TTextureDictionary;
    AAnimations: specialize THashMap<string, TAnimation>;
  public
    constructor Create(BRenderer: PSDL_Renderer);
    destructor Destroy; override;
    procedure AddTexture(textureId: string; PTexture: PSDL_Texture);
    procedure LoadTexture(textureId: string; Path: pansichar);
    function GetTexture(textureId: string): PSDL_Texture;
    procedure AddAnimation(animationId: string; animationValue: TAnimation);
    function GetAnimation(animationId: string): TAnimation;
  end;


implementation

constructor TAssetManager.Create(BRenderer: PSDL_Renderer);
begin
  ASdlRenderer := BRenderer;
  ATextures := TTextureDictionary.Create;
  AAnimations := specialize THashMap<string, TAnimation>.Create;
end;

destructor TAssetManager.Destroy;
var
  TextureValue: PSDL_Texture;
  AnimationValue: TAnimation;
begin
  for TextureValue in ATextures.Values do
  begin
    LogDebug('ASSET_MANAGER::Destroy Textuer');

    SDL_DestroyTexture(TextureValue);
  end;
  ATextures.Free;

  for AnimationValue in AAnimations.Values do
  begin
    AnimationValue.Free;
  end;

  AAnimations.Free;

end;

procedure TAssetManager.AddTexture(textureId: string; PTexture: PSDL_Texture);
begin
  LogDebug('AssetManager::AddTexture::' + textureId);
  if PTexture <> nil then
  begin
    ATextures.Add(textureId, PTexture);
  end
  else
  begin
    WriteLn(' Unable add texture: ', textureId);
  end;
end;

function TAssetManager.GetTexture(textureId: string): PSDL_Texture;
var
  SearchedValue: PSDL_Texture;
begin
  if ATextures.TryGetValue(textureId, SearchedValue) then
  begin
    Result := SearchedValue;
  end
  else
  begin
    Result := nil;
  end;
end;


procedure TAssetManager.LoadTexture(textureId: string; Path: pansichar);
var
  ATexture: PSDL_Texture;
begin
  LogDebug(' Add New Texture : ' + textureId);
  ATexture := IMG_LoadTexture(ASdlRenderer, Path);
  Self.AddTexture(textureId, ATexture);
end;

procedure TAssetManager.AddAnimation(animationId: string; animationValue: TAnimation);
begin
  if animationValue <> nil then
  begin
    AAnimations.Add(animationId, animationValue);
  end;
end;

function TAssetManager.GetAnimation(animationId: string): TAnimation;
var
  SearchedValue: TAnimation;
begin
  if AAnimations.TryGetValue(animationId, SearchedValue) then
    Result := SearchedValue
  else
    Result := nil;
end;

end.
