;;; sengoku-data.el --- Static data for Sengoku -*- lexical-binding: t; -*-

;; Copyright (C) 2026 dkc

;; Author: dkc
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: games

;;; Commentary:

;; Ordered, read-only-by-convention data translated from plugin/sengoku.vim.
;; Vectors provide stable integer indices for future Vim save compatibility.

;;; Code:

(defconst sengoku-data-save-version 5
  "Vim save-format version represented by the source data.")

(defconst sengoku-data-ranks
  ["無位" "従五位下" "従四位下" "従三位" "参議" "権大納言" "内大臣" "関白"]
  "Court ranks in ascending order.")

(defconst sengoku-data-map-cell-width 10
  "Display width of one strategic-map cell in the Vim implementation.")
(defconst sengoku-data-map-rows 7
  "Number of strategic-map rows.")
(defconst sengoku-data-map-columns 11
  "Number of strategic-map columns.")
(defconst sengoku-data-log-display-count 8
  "Number of log entries shown below the Vim strategic map.")

(defconst sengoku-data-siege-rows 9
  "Number of siege-map rows.")
(defconst sengoku-data-siege-columns 12
  "Number of siege-map columns.")
(defconst sengoku-data-siege-keep [4 8]
  "Siege-map row and column of the keep.")
(defconst sengoku-data-siege-gate [4 6]
  "Siege-map row and column of the gate.")
(defconst sengoku-data-siege-move-points 3
  "Movement points available to a siege unit each day.")
(defconst sengoku-data-siege-max-days 30
  "Maximum duration of a siege.")
(defconst sengoku-data-siege-wall-bounds [2 6 6 10]
  "Inclusive top, bottom, left, and right wall coordinates.")
(defconst sengoku-data-siege-wall-hit-points 200
  "Initial hit points of each wall section.")
(defconst sengoku-data-siege-gate-hit-points 120
  "Initial hit points of the gate.")
(defconst sengoku-data-siege-forest-count 7
  "Number of randomly planted siege-map forest cells.")
(defconst sengoku-data-siege-forest-columns [2 3 4 5]
  "Columns in which siege-map forests may be planted.")
(defconst sengoku-data-siege-attacker-labels ["１" "２" "３" "４" "５"]
  "Labels for attacker units, in deployment order.")
(defconst sengoku-data-siege-defender-labels ["甲" "乙" "丙" "丁" "戊"]
  "Labels for defender units, in deployment order.")
(defconst sengoku-data-siege-attacker-spots
  [[4 0] [2 0] [6 0] [3 0] [5 0]]
  "Initial attacker deployment coordinates.")
(defconst sengoku-data-siege-defender-spots
  [[4 7] [3 7] [5 7] [3 9] [5 9]]
  "Initial defender deployment coordinates.")

(defconst sengoku-data-clans
  [
   (:name "南部" :abbreviation "南部" :daimyo "南部晴政" :war 62 :politics 58 :charisma 60)
   (:name "伊達" :abbreviation "伊達" :daimyo "伊達晴宗" :war 64 :politics 72 :charisma 68)
   (:name "相馬" :abbreviation "相馬" :daimyo "相馬盛胤" :war 75 :politics 62 :charisma 66)
   (:name "蘆名" :abbreviation "蘆名" :daimyo "蘆名盛氏" :war 70 :politics 74 :charisma 70)
   (:name "佐竹" :abbreviation "佐竹" :daimyo "佐竹義昭" :war 68 :politics 70 :charisma 66)
   (:name "北条" :abbreviation "北条" :daimyo "北条氏康" :war 84 :politics 92 :charisma 82)
   (:name "里見" :abbreviation "里見" :daimyo "里見義堯" :war 72 :politics 62 :charisma 68)
   (:name "上杉" :abbreviation "上杉" :daimyo "上杉謙信" :war 100 :politics 62 :charisma 88)
   (:name "本願寺" :abbreviation "本願" :daimyo "本願寺顕如" :war 74 :politics 70 :charisma 96)
   (:name "朝倉" :abbreviation "朝倉" :daimyo "朝倉義景" :war 52 :politics 66 :charisma 60)
   (:name "武田" :abbreviation "武田" :daimyo "武田信玄" :war 97 :politics 90 :charisma 92)
   (:name "今川" :abbreviation "今川" :daimyo "今川義元" :war 74 :politics 82 :charisma 74)
   (:name "松平" :abbreviation "松平" :daimyo "松平元康" :war 84 :politics 86 :charisma 86)
   (:name "織田" :abbreviation "織田" :daimyo "織田信長" :war 94 :politics 88 :charisma 80)
   (:name "斎藤" :abbreviation "斎藤" :daimyo "斎藤義龍" :war 78 :politics 70 :charisma 66)
   (:name "北畠" :abbreviation "北畠" :daimyo "北畠具教" :war 76 :politics 60 :charisma 64)
   (:name "浅井" :abbreviation "浅井" :daimyo "浅井長政" :war 80 :politics 68 :charisma 76)
   (:name "三好" :abbreviation "三好" :daimyo "三好長慶" :war 74 :politics 78 :charisma 70)
   (:name "松永" :abbreviation "松永" :daimyo "松永久秀" :war 72 :politics 82 :charisma 40)
   (:name "雑賀" :abbreviation "雑賀" :daimyo "鈴木佐大夫" :war 82 :politics 50 :charisma 58)
   (:name "波多野" :abbreviation "波多" :daimyo "波多野晴通" :war 58 :politics 60 :charisma 58)
   (:name "赤松" :abbreviation "赤松" :daimyo "赤松義祐" :war 50 :politics 56 :charisma 50)
   (:name "山名" :abbreviation "山名" :daimyo "山名祐豊" :war 54 :politics 58 :charisma 54)
   (:name "浦上" :abbreviation "浦上" :daimyo "浦上宗景" :war 62 :politics 60 :charisma 56)
   (:name "尼子" :abbreviation "尼子" :daimyo "尼子晴久" :war 78 :politics 72 :charisma 70)
   (:name "毛利" :abbreviation "毛利" :daimyo "毛利元就" :war 86 :politics 94 :charisma 84)
   (:name "長宗我部" :abbreviation "長宗" :daimyo "長宗我部国親" :war 80 :politics 74 :charisma 74)
   (:name "河野" :abbreviation "河野" :daimyo "河野通宣" :war 52 :politics 56 :charisma 54)
   (:name "大友" :abbreviation "大友" :daimyo "大友宗麟" :war 74 :politics 80 :charisma 72)
   (:name "龍造寺" :abbreviation "龍造" :daimyo "龍造寺隆信" :war 84 :politics 66 :charisma 58)
   (:name "伊東" :abbreviation "伊東" :daimyo "伊東義祐" :war 58 :politics 62 :charisma 56)
   (:name "島津" :abbreviation "島津" :daimyo "島津貴久" :war 84 :politics 74 :charisma 78)
   ]
  "The 32 clans, in save-compatible index order.")

(defconst sengoku-data-provinces
  [
   (:name "陸奥" :owner "南部" :grid-row 0 :grid-column 10
	  :adjacent ["出羽" "磐城"] :koku 380 :commerce 25 :soldiers 3500 :special nil)
   (:name "出羽" :owner "伊達" :grid-row 0 :grid-column 9
	  :adjacent ["陸奥" "会津" "越後"] :koku 400 :commerce 30 :soldiers 4000 :special nil)
   (:name "磐城" :owner "相馬" :grid-row 1 :grid-column 10
	  :adjacent ["陸奥" "会津" "常陸"] :koku 350 :commerce 30 :soldiers 8000 :special nil)
   (:name "会津" :owner "蘆名" :grid-row 1 :grid-column 8
	  :adjacent ["出羽" "越後" "常陸" "上野" "磐城"] :koku 380 :commerce 30 :soldiers 4000 :special nil)
   (:name "常陸" :owner "佐竹" :grid-row 1 :grid-column 9
	  :adjacent ["会津" "上野" "武蔵" "安房" "磐城"] :koku 420 :commerce 35 :soldiers 4200 :special nil)
   (:name "上野" :owner "北条" :grid-row 2 :grid-column 8
	  :adjacent ["会津" "常陸" "武蔵" "信濃" "越後"] :koku 400 :commerce 35 :soldiers 4000 :special nil)
   (:name "武蔵" :owner "北条" :grid-row 2 :grid-column 9
	  :adjacent ["常陸" "上野" "相模" "甲斐" "安房"] :koku 520 :commerce 60 :soldiers 5500 :special nil)
   (:name "相模" :owner "北条" :grid-row 3 :grid-column 10
	  :adjacent ["武蔵" "甲斐" "駿河" "安房"] :koku 400 :commerce 65 :soldiers 5000 :special nil)
   (:name "安房" :owner "里見" :grid-row 4 :grid-column 10
	  :adjacent ["常陸" "武蔵" "相模"] :koku 340 :commerce 40 :soldiers 3800 :special nil)
   (:name "越後" :owner "上杉" :grid-row 1 :grid-column 7
	  :adjacent ["出羽" "会津" "上野" "信濃" "越中"] :koku 450 :commerce 45 :soldiers 6000 :special nil)
   (:name "越中" :owner "上杉" :grid-row 1 :grid-column 6
	  :adjacent ["越後" "加賀" "美濃"] :koku 340 :commerce 35 :soldiers 3200 :special nil)
   (:name "加賀" :owner "本願寺" :grid-row 1 :grid-column 5
	  :adjacent ["越中" "越前"] :koku 400 :commerce 45 :soldiers 4500 :special (:loyalty 90))
   (:name "越前" :owner "朝倉" :grid-row 1 :grid-column 4
	  :adjacent ["加賀" "近江" "美濃"] :koku 420 :commerce 50 :soldiers 4200 :special nil)
   (:name "信濃" :owner "武田" :grid-row 2 :grid-column 6
	  :adjacent ["越後" "上野" "甲斐" "美濃" "三河" "遠江"] :koku 430 :commerce 30 :soldiers 5000 :special nil)
   (:name "甲斐" :owner "武田" :grid-row 2 :grid-column 7
	  :adjacent ["信濃" "武蔵" "相模" "駿河"] :koku 360 :commerce 35 :soldiers 5500 :special nil)
   (:name "駿河" :owner "今川" :grid-row 3 :grid-column 9
	  :adjacent ["甲斐" "相模" "遠江"] :koku 400 :commerce 55 :soldiers 4800 :special nil)
   (:name "遠江" :owner "今川" :grid-row 3 :grid-column 8
	  :adjacent ["駿河" "信濃" "三河"] :koku 360 :commerce 40 :soldiers 3500 :special nil)
   (:name "三河" :owner "松平" :grid-row 3 :grid-column 7
	  :adjacent ["遠江" "信濃" "尾張"] :koku 380 :commerce 40 :soldiers 4200 :special nil)
   (:name "尾張" :owner "織田" :grid-row 3 :grid-column 6
	  :adjacent ["三河" "美濃" "伊勢"] :koku 540 :commerce 65 :soldiers 5800 :special nil)
   (:name "美濃" :owner "斎藤" :grid-row 2 :grid-column 5
	  :adjacent ["信濃" "越中" "越前" "尾張" "近江"] :koku 470 :commerce 45 :soldiers 4800 :special nil)
   (:name "伊勢" :owner "北畠" :grid-row 4 :grid-column 6
	  :adjacent ["尾張" "近江" "大和" "紀伊"] :koku 440 :commerce 55 :soldiers 4000 :special nil)
   (:name "近江" :owner "浅井" :grid-row 2 :grid-column 4
	  :adjacent ["越前" "美濃" "伊勢" "山城"] :koku 480 :commerce 60 :soldiers 4500 :special nil)
   (:name "山城" :owner "三好" :grid-row 3 :grid-column 5
	  :adjacent ["近江" "丹波" "摂津" "大和"] :koku 400 :commerce 95 :soldiers 4200 :special nil)
   (:name "摂津" :owner "三好" :grid-row 3 :grid-column 4
	  :adjacent ["山城" "丹波" "播磨" "大和" "紀伊" "阿波"] :koku 450 :commerce 90 :soldiers 4500 :special (:port 1))
   (:name "大和" :owner "松永" :grid-row 4 :grid-column 5
	  :adjacent ["山城" "摂津" "伊勢" "紀伊"] :koku 400 :commerce 50 :soldiers 4000 :special nil)
   (:name "紀伊" :owner "雑賀" :grid-row 5 :grid-column 5
	  :adjacent ["大和" "摂津" "伊勢" "阿波"] :koku 360 :commerce 50 :soldiers 4200 :special (:guns 80))
   (:name "丹波" :owner "波多野" :grid-row 2 :grid-column 3
	  :adjacent ["山城" "摂津" "但馬" "播磨"] :koku 340 :commerce 35 :soldiers 3200 :special nil)
   (:name "播磨" :owner "赤松" :grid-row 3 :grid-column 3
	  :adjacent ["丹波" "但馬" "摂津" "備前"] :koku 420 :commerce 50 :soldiers 3800 :special nil)
   (:name "但馬" :owner "山名" :grid-row 2 :grid-column 2
	  :adjacent ["丹波" "播磨" "出雲"] :koku 300 :commerce 35 :soldiers 3000 :special nil)
   (:name "備前" :owner "浦上" :grid-row 3 :grid-column 2
	  :adjacent ["播磨" "出雲" "安芸"] :koku 380 :commerce 45 :soldiers 3600 :special nil)
   (:name "出雲" :owner "尼子" :grid-row 2 :grid-column 1
	  :adjacent ["但馬" "備前" "安芸" "周防"] :koku 400 :commerce 40 :soldiers 4800 :special nil)
   (:name "安芸" :owner "毛利" :grid-row 3 :grid-column 1
	  :adjacent ["備前" "出雲" "周防" "伊予"] :koku 400 :commerce 50 :soldiers 5200 :special nil)
   (:name "周防" :owner "毛利" :grid-row 4 :grid-column 0
	  :adjacent ["安芸" "出雲" "筑前" "伊予"] :koku 380 :commerce 55 :soldiers 4200 :special nil)
   (:name "阿波" :owner "三好" :grid-row 4 :grid-column 4
	  :adjacent ["摂津" "紀伊" "土佐" "伊予"] :koku 380 :commerce 45 :soldiers 4500 :special nil)
   (:name "土佐" :owner "長宗我部" :grid-row 5 :grid-column 3
	  :adjacent ["阿波" "伊予"] :koku 350 :commerce 35 :soldiers 4500 :special nil)
   (:name "伊予" :owner "河野" :grid-row 4 :grid-column 2
	  :adjacent ["阿波" "土佐" "安芸" "周防" "豊後"] :koku 360 :commerce 40 :soldiers 3500 :special nil)
   (:name "筑前" :owner "大友" :grid-row 5 :grid-column 0
	  :adjacent ["周防" "肥前" "豊後"] :koku 400 :commerce 75 :soldiers 4200 :special (:port 1))
   (:name "豊後" :owner "大友" :grid-row 5 :grid-column 1
	  :adjacent ["筑前" "伊予" "日向"] :koku 420 :commerce 55 :soldiers 5000 :special (:guns 30 :port 1))
   (:name "肥前" :owner "龍造寺" :grid-row 6 :grid-column 0
	  :adjacent ["筑前" "薩摩"] :koku 380 :commerce 45 :soldiers 4200 :special (:port 1))
   (:name "日向" :owner "伊東" :grid-row 6 :grid-column 2
	  :adjacent ["豊後" "薩摩"] :koku 340 :commerce 30 :soldiers 3500 :special nil)
   (:name "薩摩" :owner "島津" :grid-row 6 :grid-column 1
	  :adjacent ["肥前" "日向"] :koku 380 :commerce 35 :soldiers 5200 :special (:guns 30 :port 1))
   ]
  "The 41 province definitions, in save-compatible index order.")

(defconst sengoku-data-retainers
  [
   [
    (:name "北信愛" :war 60 :politics 68)
    (:name "石川高信" :war 62 :politics 55)
    (:name "九戸政実" :war 78 :politics 40)
    (:name "八戸政栄" :war 65 :politics 60)
    (:name "東政勝" :war 58 :politics 55)
    (:name "桜庭直綱" :war 55 :politics 50)
    (:name "南部信直" :war 60 :politics 70)
    ]
   [
    (:name "伊達実元" :war 68 :politics 60)
    (:name "鬼庭良直" :war 70 :politics 55)
    (:name "中野宗時" :war 45 :politics 68)
    (:name "牧野久仲" :war 50 :politics 60)
    (:name "桑折景長" :war 55 :politics 65)
    (:name "白石宗実" :war 66 :politics 50)
    (:name "亘理元宗" :war 64 :politics 58)
    ]
   [
    (:name "相馬義胤" :war 76 :politics 62)
    (:name "相馬隆胤" :war 70 :politics 50)
    (:name "木幡継清" :war 64 :politics 58)
    (:name "青田顕治" :war 60 :politics 54)
    (:name "岡田義胤" :war 58 :politics 52)
    (:name "泉胤政" :war 56 :politics 50)
    (:name "水谷胤重" :war 62 :politics 46)
    ]
   [
    (:name "金上盛備" :war 60 :politics 72)
    (:name "富田氏実" :war 58 :politics 52)
    (:name "佐瀬種常" :war 55 :politics 50)
    (:name "平田舜範" :war 52 :politics 58)
    (:name "松本氏輔" :war 56 :politics 48)
    (:name "猪苗代盛国" :war 58 :politics 45)
    (:name "蘆名盛興" :war 55 :politics 50)
    ]
   [
    (:name "真壁氏幹" :war 78 :politics 40)
    (:name "和田昭為" :war 50 :politics 70)
    (:name "小貫頼久" :war 45 :politics 65)
    (:name "岡本禅哲" :war 48 :politics 66)
    (:name "東義堅" :war 54 :politics 56)
    (:name "佐竹義廉" :war 55 :politics 52)
    (:name "小野崎昭通" :war 52 :politics 54)
    ]
   [
    (:name "北条綱成" :war 88 :politics 55)
    (:name "大道寺政繁" :war 70 :politics 72)
    (:name "風魔小太郎" :war 75 :politics 30)
    (:name "北条幻庵" :war 55 :politics 85)
    (:name "北条氏照" :war 72 :politics 70)
    (:name "北条氏邦" :war 70 :politics 65)
    (:name "松田憲秀" :war 52 :politics 68)
    ]
   [
    (:name "正木時茂" :war 82 :politics 45)
    (:name "正木時忠" :war 68 :politics 50)
    (:name "里見義弘" :war 70 :politics 60)
    (:name "里見義頼" :war 58 :politics 62)
    (:name "正木憲時" :war 64 :politics 48)
    (:name "正木頼忠" :war 60 :politics 55)
    (:name "土岐為頼" :war 56 :politics 50)
    ]
   [
    (:name "柿崎景家" :war 85 :politics 40)
    (:name "直江景綱" :war 60 :politics 80)
    (:name "宇佐美定満" :war 72 :politics 75)
    (:name "本庄繁長" :war 84 :politics 45)
    (:name "色部勝長" :war 70 :politics 55)
    (:name "斎藤朝信" :war 75 :politics 68)
    (:name "甘粕景持" :war 74 :politics 50)
    ]
   [
    (:name "下間頼廉" :war 80 :politics 60)
    (:name "七里頼周" :war 65 :politics 55)
    (:name "下間頼照" :war 60 :politics 58)
    (:name "下間頼旦" :war 58 :politics 50)
    (:name "杉浦玄任" :war 68 :politics 52)
    (:name "願証寺証恵" :war 50 :politics 60)
    (:name "下間光頼" :war 52 :politics 62)
    ]
   [
    (:name "朝倉宗滴" :war 88 :politics 70)
    (:name "真柄直隆" :war 80 :politics 25)
    (:name "山崎吉家" :war 65 :politics 60)
    (:name "朝倉景鏡" :war 60 :politics 58)
    (:name "朝倉景健" :war 62 :politics 55)
    (:name "魚住景固" :war 50 :politics 62)
    (:name "前波吉継" :war 45 :politics 58)
    ]
   [
    (:name "山本勘助" :war 75 :politics 80)
    (:name "馬場信春" :war 85 :politics 70)
    (:name "山県昌景" :war 90 :politics 55)
    (:name "高坂昌信" :war 80 :politics 72)
    (:name "内藤昌豊" :war 78 :politics 70)
    (:name "武田信繁" :war 76 :politics 78)
    (:name "真田幸隆" :war 72 :politics 85)
    ]
   [
    (:name "太原雪斎" :war 70 :politics 95)
    (:name "朝比奈泰能" :war 68 :politics 65)
    (:name "岡部元信" :war 78 :politics 45)
    (:name "井伊直盛" :war 62 :politics 50)
    (:name "鵜殿長照" :war 58 :politics 48)
    (:name "関口親永" :war 50 :politics 62)
    (:name "朝比奈泰朝" :war 66 :politics 55)
    ]
   [
    (:name "本多忠勝" :war 92 :politics 40)
    (:name "酒井忠次" :war 75 :politics 70)
    (:name "石川数正" :war 60 :politics 75)
    (:name "榊原康政" :war 88 :politics 60)
    (:name "鳥居元忠" :war 70 :politics 55)
    (:name "大久保忠世" :war 72 :politics 52)
    (:name "服部半蔵" :war 78 :politics 40)
    ]
   [
    (:name "柴田勝家" :war 88 :politics 55)
    (:name "木下藤吉郎" :war 75 :politics 90)
    (:name "明智光秀" :war 82 :politics 85)
    (:name "丹羽長秀" :war 70 :politics 78)
    (:name "前田利家" :war 80 :politics 60)
    (:name "佐々成政" :war 76 :politics 55)
    (:name "池田恒興" :war 68 :politics 58)
    ]
   [
    (:name "竹中半兵衛" :war 85 :politics 80)
    (:name "安藤守就" :war 65 :politics 55)
    (:name "稲葉一鉄" :war 75 :politics 60)
    (:name "氏家卜全" :war 68 :politics 58)
    (:name "日根野弘就" :war 70 :politics 48)
    (:name "斎藤利三" :war 72 :politics 60)
    (:name "不破光治" :war 60 :politics 62)
    ]
   [
    (:name "鳥屋尾満栄" :war 55 :politics 65)
    (:name "大宮含忍斎" :war 60 :politics 50)
    (:name "藤方朝成" :war 58 :politics 55)
    (:name "木造具政" :war 52 :politics 56)
    (:name "北畠具房" :war 40 :politics 50)
    (:name "田丸具忠" :war 54 :politics 52)
    (:name "長野具藤" :war 50 :politics 45)
    ]
   [
    (:name "磯野員昌" :war 82 :politics 45)
    (:name "遠藤直経" :war 75 :politics 60)
    (:name "海北綱親" :war 78 :politics 55)
    (:name "赤尾清綱" :war 72 :politics 58)
    (:name "雨森清貞" :war 65 :politics 50)
    (:name "浅井久政" :war 40 :politics 55)
    (:name "阿閉貞征" :war 55 :politics 52)
    ]
   [
    (:name "三好実休" :war 75 :politics 65)
    (:name "十河一存" :war 85 :politics 40)
    (:name "安宅冬康" :war 65 :politics 70)
    (:name "三好長逸" :war 68 :politics 62)
    (:name "三好政康" :war 66 :politics 58)
    (:name "岩成友通" :war 64 :politics 60)
    (:name "篠原長房" :war 62 :politics 75)
    ]
   [
    (:name "松永長頼" :war 75 :politics 60)
    (:name "竹内秀勝" :war 60 :politics 55)
    (:name "海老名家秀" :war 55 :politics 50)
    (:name "松永久通" :war 50 :politics 48)
    (:name "高山友照" :war 58 :politics 60)
    (:name "結城忠正" :war 55 :politics 58)
    (:name "四手井家保" :war 52 :politics 50)
    ]
   [
    (:name "雑賀孫市" :war 92 :politics 35)
    (:name "土橋守重" :war 70 :politics 55)
    (:name "的場昌長" :war 68 :politics 45)
    (:name "佐武義昌" :war 72 :politics 40)
    (:name "岡吉正" :war 60 :politics 42)
    (:name "土橋若大夫" :war 62 :politics 48)
    (:name "太田左近" :war 65 :politics 58)
    ]
   [
    (:name "波多野秀治" :war 65 :politics 60)
    (:name "荒木氏綱" :war 62 :politics 55)
    (:name "籾井教業" :war 72 :politics 35)
    (:name "赤井直正" :war 84 :politics 50)
    (:name "波多野秀尚" :war 58 :politics 50)
    (:name "波多野宗長" :war 55 :politics 48)
    (:name "香西元成" :war 63 :politics 48)
    ]
   [
    (:name "別所安治" :war 62 :politics 55)
    (:name "赤松政秀" :war 58 :politics 50)
    (:name "宇野政頼" :war 55 :politics 52)
    (:name "小寺政職" :war 45 :politics 58)
    (:name "黒田職隆" :war 60 :politics 68)
    (:name "別所就治" :war 64 :politics 56)
    (:name "三木通秋" :war 52 :politics 50)
    ]
   [
    (:name "山名豊数" :war 52 :politics 50)
    (:name "垣屋続成" :war 58 :politics 55)
    (:name "太田垣輝延" :war 55 :politics 52)
    (:name "田結庄是義" :war 54 :politics 50)
    (:name "八木豊信" :war 53 :politics 51)
    (:name "垣屋光成" :war 56 :politics 54)
    (:name "山名豊国" :war 50 :politics 55)
    ]
   [
    (:name "宇喜多直家" :war 80 :politics 88)
    (:name "明石行雄" :war 62 :politics 58)
    (:name "花房正幸" :war 65 :politics 50)
    (:name "長船貞親" :war 58 :politics 65)
    (:name "戸川秀安" :war 60 :politics 62)
    (:name "岡家利" :war 57 :politics 55)
    (:name "延原景能" :war 55 :politics 48)
    ]
   [
    (:name "山中幸盛" :war 88 :politics 50)
    (:name "立原久綱" :war 65 :politics 70)
    (:name "宇山久兼" :war 55 :politics 72)
    (:name "亀井秀綱" :war 52 :politics 64)
    (:name "牛尾幸清" :war 60 :politics 50)
    (:name "米原綱寛" :war 62 :politics 55)
    (:name "秋上宗信" :war 58 :politics 48)
    ]
   [
    (:name "吉川元春" :war 90 :politics 65)
    (:name "小早川隆景" :war 80 :politics 90)
    (:name "毛利隆元" :war 65 :politics 75)
    (:name "福原貞俊" :war 60 :politics 72)
    (:name "口羽通良" :war 58 :politics 70)
    (:name "熊谷信直" :war 72 :politics 55)
    (:name "桂元澄" :war 62 :politics 64)
    ]
   [
    (:name "長宗我部元親" :war 88 :politics 80)
    (:name "吉田孝頼" :war 70 :politics 75)
    (:name "福留親政" :war 75 :politics 45)
    (:name "香宗我部親泰" :war 68 :politics 66)
    (:name "久武昌源" :war 58 :politics 60)
    (:name "吉田重俊" :war 62 :politics 55)
    (:name "谷忠澄" :war 50 :politics 70)
    ]
   [
    (:name "村上武吉" :war 85 :politics 55)
    (:name "平岡房実" :war 55 :politics 65)
    (:name "大野直之" :war 60 :politics 45)
    (:name "村上通康" :war 75 :politics 58)
    (:name "土居清良" :war 70 :politics 60)
    (:name "戒能通森" :war 56 :politics 48)
    (:name "河野通直" :war 50 :politics 56)
    ]
   [
    (:name "戸次鑑連" :war 95 :politics 75)
    (:name "臼杵鑑速" :war 65 :politics 75)
    (:name "吉弘鑑理" :war 70 :politics 65)
    (:name "高橋鑑種" :war 68 :politics 60)
    (:name "志賀親守" :war 56 :politics 58)
    (:name "田原親賢" :war 60 :politics 62)
    (:name "佐伯惟教" :war 66 :politics 54)
    ]
   [
    (:name "鍋島直茂" :war 85 :politics 85)
    (:name "成松信勝" :war 80 :politics 40)
    (:name "百武賢兼" :war 78 :politics 38)
    (:name "木下昌直" :war 72 :politics 36)
    (:name "江里口信常" :war 74 :politics 35)
    (:name "円城寺信胤" :war 70 :politics 37)
    (:name "龍造寺長信" :war 58 :politics 60)
    ]
   [
    (:name "伊東祐安" :war 60 :politics 50)
    (:name "落合兼朝" :war 55 :politics 52)
    (:name "川崎祐長" :war 58 :politics 48)
    (:name "山田宗昌" :war 62 :politics 55)
    (:name "伊東義益" :war 50 :politics 58)
    (:name "長倉祐政" :war 54 :politics 46)
    (:name "米良矩重" :war 52 :politics 44)
    ]
   [
    (:name "島津義久" :war 75 :politics 88)
    (:name "島津義弘" :war 96 :politics 60)
    (:name "島津歳久" :war 72 :politics 75)
    (:name "島津家久" :war 90 :politics 55)
    (:name "新納忠元" :war 80 :politics 58)
    (:name "伊集院忠朗" :war 64 :politics 70)
    (:name "川上久朗" :war 66 :politics 56)
    ]
   ]
  "Seven retainers per clan, aligned with `sengoku-data-clans`.
The daimyo is not included here; core initialization prepends the daimyo.")

(defconst sengoku-data-items
  [
   (:name "九十九髪茄子" :price 3000 :culture 15)
   (:name "平蜘蛛釜" :price 2500 :culture 12)
   (:name "初花肩衝" :price 2000 :culture 10)
   (:name "新田肩衝" :price 2000 :culture 10)
   (:name "楢柴肩衝" :price 1800 :culture 9)
   (:name "三日月茶壷" :price 1500 :culture 8)
   (:name "松島の壷" :price 1200 :culture 7)
   (:name "珠光小茄子" :price 1000 :culture 6)
   (:name "曜変天目" :price 800 :culture 5)
   (:name "青磁千声" :price 600 :culture 4)
   ]
  "The ten tea utensils, in save-compatible index order.")

(defconst sengoku-data-culture-base
  [5 5 5 5 5 5 5 5 5 5 5 25 5 5 5 15 5 20 20 5 5 5 5 5 5 5 5 5 15 5 5 5]
  "Base culture by clan index, before item bonuses are applied.")

(defconst sengoku-data-initial-item-owners
  [18 18 -1 -1 -1 17 11 -1 -1 -1]
  "Initial owner clan index for each item, or -1 when unowned.")

(defconst sengoku-data-clan-count 32
  "Expected number of clans.")
(defconst sengoku-data-province-count 41
  "Expected number of provinces.")
(defconst sengoku-data-retainers-per-clan 7
  "Expected number of retainers per clan, excluding the daimyo.")
(defconst sengoku-data-item-count 10
  "Expected number of tea utensils.")

(provide 'sengoku-data)

;;; sengoku-data.el ends here
