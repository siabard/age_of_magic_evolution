unit asset_manager;

{$mode ObjFPC}{$H+}

interface


uses
  Classes, SysUtils, sdl2, sdl2_image,
  Generics.Collections, LogUtil, animation, atlas;

type

  TTextureDictionary = specialize TDictionary<string, PSDL_Texture>;

  TAssetManager = class
  private
    FSdlRenderer: PSDL_Renderer;
    FTextures: TTextureDictionary;
    FAnimations: specialize THashMap<string, TAnimation>;
    FATlas: specialize THashMap<string, TAtlas>;
  public
    constructor Create(BRenderer: PSDL_Renderer);
    destructor Destroy; override;
    procedure AddTexture(textureId: string; PTexture: PSDL_Texture);
    procedure LoadTexture(textureId: string; Path: pansichar);
    function GetTexture(textureId: string): PSDL_Texture;
    procedure AddAnimation(animationId: string; animationValue: TAnimation);
    function GetAnimation(animationId: string): TAnimation;
    procedure AddAtlas(atlasId: string; textureId: string; tile_width: integer;
      tile_height: integer);
    function GetAtlas(atlasId: String): TAtlas;
  end;


implementation

constructor TAssetManager.Create(BRenderer: PSDL_Renderer);
begin
  FSdlRenderer := BRenderer;
  FTextures := TTextureDictionary.Create;
  FAnimations := specialize THashMap<string, TAnimation>.Create;
  FATlas := specialize THashMap<string, TAtlas>.Create;
end;

destructor TAssetManager.Destroy;
var
  TextureValue: PSDL_Texture;
  AnimationValue: TAnimation;
  AtlasValue: TAtlas;
begin
  for AtlasValue in FATlas.Values do
  begin
    AtlasValue.Free;
  end;
  FATlas.Free;

  for AnimationValue in FAnimations.Values do
  begin
    AnimationValue.Free;
  end;

  FAnimations.Free;

  for TextureValue in FTextures.Values do
  begin
    LogDebug('ASSET_MANAGER::Destroy Textuer');

    SDL_DestroyTexture(TextureValue);
  end;
  FTextures.Free;

end;

procedure TAssetManager.AddTexture(textureId: string; PTexture: PSDL_Texture);
begin
  LogDebug('AssetManager::AddTexture::' + textureId);
  if PTexture <> nil then
  begin
    FTextures.Add(textureId, PTexture);
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
  if FTextures.TryGetValue(textureId, SearchedValue) then
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
  ATexture := IMG_LoadTexture(FSdlRenderer, Path);
  Self.AddTexture(textureId, ATexture);
end;

procedure TAssetManager.AddAnimation(animationId: string; animationValue: TAnimation);
begin
  if animationValue <> nil then
  begin
    FAnimations.Add(animationId, animationValue);
  end;
end;

function TAssetManager.GetAnimation(animationId: string): TAnimation;
var
  SearchedValue: TAnimation;
begin
  if FAnimations.TryGetValue(animationId, SearchedValue) then
    Result := SearchedValue
  else
    Result := nil;
end;

procedure TAssetManager.AddAtlas(atlasId: string; textureId: string;
  tile_width: integer; tile_height: integer);
var
  AtlasValue: TAtlas;
  texture_width: integer;
  texture_height: integer;
  texture_for_atlas: PSDL_Texture;
  texture_format: UInt32;
  texture_access: integer;
begin
  AtlasValue := TAtlas.Create;
  AtlasValue.TextureName := textureId;
  texture_for_atlas := GetTexture(textureId);
  if texture_for_atlas <> nil then
  begin
    SDL_QueryTexture(texture_for_atlas, @texture_format, @texture_access,
      @texture_width, @texture_height);
    AtlasValue.MakeAtlas(texture_width, texture_height, tile_width, tile_height);
    FATlas.Add(atlasId, AtlasValue);
  end;

end;

function TAssetManager.GetAtlas(atlasId: String): TAtlas;
var
  SearchedValue: TAtlas;
begin
  if FATlas.TryGetValue(atlasId, SearchedValue) then
    Result := SearchedValue
  else
    Result := nil;
end;

end.
