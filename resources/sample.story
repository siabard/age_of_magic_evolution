# story 파일: sample.story

[story]
id = intro
text = "당신은 어두운 동굴에서 깨어납니다. 바닥에 횃불이 보입니다."
choices = pick_torch, leave_torch

[choice:pick_torch]
text = "횃불을 집는다."
next = torch_taken
set = has_torch=true

[choice:leave_torch]
text = "그냥 지나친다."
next = no_torch

[story]
id = torch_taken
text = "당신은 횃불을 집었습니다. 이제 주변을 볼 수 있습니다."
choices = continue_cave

[story]
id = no_torch
text = "당신은 어둠 속을 더듬으며 나아갑니다."
choices = continue_cave
set = visibility=low

[choice:continue_cave]
text = "앞으로 간다."
next = cave_branch

[story]
id = cave_branch
text = "갈림길이 나옵니다. 왼쪽과 오른쪽이 있습니다."
choices = go_left, go_right

[choice:go_left]
text = "왼쪽으로 간다."
next = encounter_slime
require = has_torch=true

[choice:go_right]
text = "오른쪽으로 간다."
next = encounter_trap

[story]
id = encounter_slime
text = "슬라임이 나타났다!"
# 전투 발생

[story]
id = encounter_trap
text = "당신은 함정을 밟았습니다. 체력이 감소합니다."
set = hp=-10
