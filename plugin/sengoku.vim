" =====================================================================
" sengoku.vim - 戦国風雲録 〜Vimで天下統一〜
" 信長の野望・武将風雲録風の戦国シミュレーションゲーム
" 全41ヶ国・32大名家 (1560年 桶狭間の年)
"
" 起動:  :Sengoku
" 再開:  :SengokuLoad
" 検証:  :SengokuTest  (マップ整合性チェック + AIのみで36ヶ月シミュレーション)
"
" Vim 8.0+ / Neovim 対応 (レガシー Vim script)
" =====================================================================
if exists('g:loaded_sengoku') && g:loaded_sengoku
  finish
endif
let g:loaded_sengoku = 1
scriptencoding utf-8

command! Sengoku call sengoku#Start()
command! SengokuLoad call sengoku#LoadStart()
command! SengokuTest call sengoku#SelfTest()
command! SengokuSiegeTest call sengoku#SiegeTest()

" ---------------------------------------------------------------------
" 定数
" ---------------------------------------------------------------------
let s:SAVEFILE = expand('~/.vim-sengoku-save.json')
let s:RANKS = ['無位', '従五位下', '従四位下', '従三位', '参議', '権大納言', '内大臣', '関白']
let s:SAVEVER = 5
let s:CELLW = 10          " マップ1マスの表示幅
let s:GRIDR = 7           " マップ行数
let s:GRIDC = 11          " マップ列数
let s:LOGN  = 8           " 画面下部に出すログ行数

" ---------------------------------------------------------------------
" データ: 大名家 (war:戦才 pol:政才 chr:魅力)
" ---------------------------------------------------------------------
func! s:Clans() abort
  return [
        \ {'name':'南部','abbr':'南部','daimyo':'南部晴政','war':62,'pol':58,'chr':60},
        \ {'name':'伊達','abbr':'伊達','daimyo':'伊達晴宗','war':64,'pol':72,'chr':68},
        \ {'name':'相馬','abbr':'相馬','daimyo':'相馬盛胤','war':75,'pol':62,'chr':66},
        \ {'name':'蘆名','abbr':'蘆名','daimyo':'蘆名盛氏','war':70,'pol':74,'chr':70},
        \ {'name':'佐竹','abbr':'佐竹','daimyo':'佐竹義昭','war':68,'pol':70,'chr':66},
        \ {'name':'北条','abbr':'北条','daimyo':'北条氏康','war':84,'pol':92,'chr':82},
        \ {'name':'里見','abbr':'里見','daimyo':'里見義堯','war':72,'pol':62,'chr':68},
        \ {'name':'上杉','abbr':'上杉','daimyo':'上杉謙信','war':100,'pol':62,'chr':88},
        \ {'name':'本願寺','abbr':'本願','daimyo':'本願寺顕如','war':74,'pol':70,'chr':96},
        \ {'name':'朝倉','abbr':'朝倉','daimyo':'朝倉義景','war':52,'pol':66,'chr':60},
        \ {'name':'武田','abbr':'武田','daimyo':'武田信玄','war':97,'pol':90,'chr':92},
        \ {'name':'今川','abbr':'今川','daimyo':'今川義元','war':74,'pol':82,'chr':74},
        \ {'name':'松平','abbr':'松平','daimyo':'松平元康','war':84,'pol':86,'chr':86},
        \ {'name':'織田','abbr':'織田','daimyo':'織田信長','war':94,'pol':88,'chr':80},
        \ {'name':'斎藤','abbr':'斎藤','daimyo':'斎藤義龍','war':78,'pol':70,'chr':66},
        \ {'name':'北畠','abbr':'北畠','daimyo':'北畠具教','war':76,'pol':60,'chr':64},
        \ {'name':'浅井','abbr':'浅井','daimyo':'浅井長政','war':80,'pol':68,'chr':76},
        \ {'name':'三好','abbr':'三好','daimyo':'三好長慶','war':74,'pol':78,'chr':70},
        \ {'name':'松永','abbr':'松永','daimyo':'松永久秀','war':72,'pol':82,'chr':40},
        \ {'name':'雑賀','abbr':'雑賀','daimyo':'鈴木佐大夫','war':82,'pol':50,'chr':58},
        \ {'name':'波多野','abbr':'波多','daimyo':'波多野晴通','war':58,'pol':60,'chr':58},
        \ {'name':'赤松','abbr':'赤松','daimyo':'赤松義祐','war':50,'pol':56,'chr':50},
        \ {'name':'山名','abbr':'山名','daimyo':'山名祐豊','war':54,'pol':58,'chr':54},
        \ {'name':'浦上','abbr':'浦上','daimyo':'浦上宗景','war':62,'pol':60,'chr':56},
        \ {'name':'尼子','abbr':'尼子','daimyo':'尼子晴久','war':78,'pol':72,'chr':70},
        \ {'name':'毛利','abbr':'毛利','daimyo':'毛利元就','war':86,'pol':94,'chr':84},
        \ {'name':'長宗我部','abbr':'長宗','daimyo':'長宗我部国親','war':80,'pol':74,'chr':74},
        \ {'name':'河野','abbr':'河野','daimyo':'河野通宣','war':52,'pol':56,'chr':54},
        \ {'name':'大友','abbr':'大友','daimyo':'大友宗麟','war':74,'pol':80,'chr':72},
        \ {'name':'龍造寺','abbr':'龍造','daimyo':'龍造寺隆信','war':84,'pol':66,'chr':58},
        \ {'name':'伊東','abbr':'伊東','daimyo':'伊東義祐','war':58,'pol':62,'chr':56},
        \ {'name':'島津','abbr':'島津','daimyo':'島津貴久','war':84,'pol':74,'chr':78},
        \ ]
endfunc

" ---------------------------------------------------------------------
" データ: 国 [名前, 大名家, 表示行, 表示列, 隣接, 石高, 商業, 兵, 特記]
" ---------------------------------------------------------------------
func! s:ProvDefs() abort
  return [
        \ ['陸奥', '南部', 0,10, ['出羽','磐城'],                   380, 25, 3500, {}],
        \ ['出羽', '伊達', 0, 9, ['陸奥','会津','越後'],            400, 30, 4000, {}],
        \ ['磐城', '相馬', 1,10, ['陸奥','会津','常陸'],            350, 30, 8000, {}],
        \ ['会津', '蘆名', 1, 8, ['出羽','越後','常陸','上野','磐城'], 380, 30, 4000, {}],
        \ ['常陸', '佐竹', 1, 9, ['会津','上野','武蔵','安房','磐城'], 420, 35, 4200, {}],
        \ ['上野', '北条', 2, 8, ['会津','常陸','武蔵','信濃','越後'], 400, 35, 4000, {}],
        \ ['武蔵', '北条', 2, 9, ['常陸','上野','相模','甲斐','安房'], 520, 60, 5500, {}],
        \ ['相模', '北条', 3,10, ['武蔵','甲斐','駿河','安房'],     400, 65, 5000, {}],
        \ ['安房', '里見', 4,10, ['常陸','武蔵','相模'],            340, 40, 3800, {}],
        \ ['越後', '上杉', 1, 7, ['出羽','会津','上野','信濃','越中'], 450, 45, 6000, {}],
        \ ['越中', '上杉', 1, 6, ['越後','加賀','美濃'],            340, 35, 3200, {}],
        \ ['加賀', '本願寺', 1, 5, ['越中','越前'],                 400, 45, 4500, {'loyal': 90}],
        \ ['越前', '朝倉', 1, 4, ['加賀','近江','美濃'],            420, 50, 4200, {}],
        \ ['信濃', '武田', 2, 6, ['越後','上野','甲斐','美濃','三河','遠江'], 430, 30, 5000, {}],
        \ ['甲斐', '武田', 2, 7, ['信濃','武蔵','相模','駿河'],     360, 35, 5500, {}],
        \ ['駿河', '今川', 3, 9, ['甲斐','相模','遠江'],            400, 55, 4800, {}],
        \ ['遠江', '今川', 3, 8, ['駿河','信濃','三河'],            360, 40, 3500, {}],
        \ ['三河', '松平', 3, 7, ['遠江','信濃','尾張'],            380, 40, 4200, {}],
        \ ['尾張', '織田', 3, 6, ['三河','美濃','伊勢'],            540, 65, 5800, {}],
        \ ['美濃', '斎藤', 2, 5, ['信濃','越中','越前','尾張','近江'], 470, 45, 4800, {}],
        \ ['伊勢', '北畠', 4, 6, ['尾張','近江','大和','紀伊'],     440, 55, 4000, {}],
        \ ['近江', '浅井', 2, 4, ['越前','美濃','伊勢','山城'],     480, 60, 4500, {}],
        \ ['山城', '三好', 3, 5, ['近江','丹波','摂津','大和'],     400, 95, 4200, {}],
        \ ['摂津', '三好', 3, 4, ['山城','丹波','播磨','大和','紀伊','阿波'], 450, 90, 4500, {'port': 1}],
        \ ['大和', '松永', 4, 5, ['山城','摂津','伊勢','紀伊'],     400, 50, 4000, {}],
        \ ['紀伊', '雑賀', 5, 5, ['大和','摂津','伊勢','阿波'],     360, 50, 4200, {'guns': 80}],
        \ ['丹波', '波多野', 2, 3, ['山城','摂津','但馬','播磨'],   340, 35, 3200, {}],
        \ ['播磨', '赤松', 3, 3, ['丹波','但馬','摂津','備前'],     420, 50, 3800, {}],
        \ ['但馬', '山名', 2, 2, ['丹波','播磨','出雲'],            300, 35, 3000, {}],
        \ ['備前', '浦上', 3, 2, ['播磨','出雲','安芸'],            380, 45, 3600, {}],
        \ ['出雲', '尼子', 2, 1, ['但馬','備前','安芸','周防'],     400, 40, 4800, {}],
        \ ['安芸', '毛利', 3, 1, ['備前','出雲','周防','伊予'],     400, 50, 5200, {}],
        \ ['周防', '毛利', 4, 0, ['安芸','出雲','筑前','伊予'],     380, 55, 4200, {}],
        \ ['阿波', '三好', 4, 4, ['摂津','紀伊','土佐','伊予'],     380, 45, 4500, {}],
        \ ['土佐', '長宗我部', 5, 3, ['阿波','伊予'],               350, 35, 4500, {}],
        \ ['伊予', '河野', 4, 2, ['阿波','土佐','安芸','周防','豊後'], 360, 40, 3500, {}],
        \ ['筑前', '大友', 5, 0, ['周防','肥前','豊後'],            400, 75, 4200, {'port': 1}],
        \ ['豊後', '大友', 5, 1, ['筑前','伊予','日向'],            420, 55, 5000, {'guns': 30, 'port': 1}],
        \ ['肥前', '龍造寺', 6, 0, ['筑前','薩摩'],                 380, 45, 4200, {'port': 1}],
        \ ['日向', '伊東', 6, 2, ['豊後','薩摩'],                   340, 30, 3500, {}],
        \ ['薩摩', '島津', 6, 1, ['肥前','日向'],                   380, 35, 5200, {'guns': 30, 'port': 1}],
        \ ]
endfunc

" ---------------------------------------------------------------------
" データ: 家臣武将 (家名 → [名前, 戦才, 政才] ×7。当主は自動で筆頭になる)
" ---------------------------------------------------------------------
func! s:GenDefs() abort
  return {
        \ '南部': [['北信愛', 60, 68], ['石川高信', 62, 55], ['九戸政実', 78, 40],
        \          ['八戸政栄', 65, 60], ['東政勝', 58, 55], ['桜庭直綱', 55, 50], ['南部信直', 60, 70]],
        \ '伊達': [['伊達実元', 68, 60], ['鬼庭良直', 70, 55], ['中野宗時', 45, 68],
        \          ['牧野久仲', 50, 60], ['桑折景長', 55, 65], ['白石宗実', 66, 50], ['亘理元宗', 64, 58]],
        \ '相馬': [['相馬義胤', 76, 62], ['相馬隆胤', 70, 50], ['木幡継清', 64, 58],
        \          ['青田顕治', 60, 54], ['岡田義胤', 58, 52], ['泉胤政', 56, 50], ['水谷胤重', 62, 46]],
        \ '蘆名': [['金上盛備', 60, 72], ['富田氏実', 58, 52], ['佐瀬種常', 55, 50],
        \          ['平田舜範', 52, 58], ['松本氏輔', 56, 48], ['猪苗代盛国', 58, 45], ['蘆名盛興', 55, 50]],
        \ '佐竹': [['真壁氏幹', 78, 40], ['和田昭為', 50, 70], ['小貫頼久', 45, 65],
        \          ['岡本禅哲', 48, 66], ['東義堅', 54, 56], ['佐竹義廉', 55, 52], ['小野崎昭通', 52, 54]],
        \ '北条': [['北条綱成', 88, 55], ['大道寺政繁', 70, 72], ['風魔小太郎', 75, 30],
        \          ['北条幻庵', 55, 85], ['北条氏照', 72, 70], ['北条氏邦', 70, 65], ['松田憲秀', 52, 68]],
        \ '里見': [['正木時茂', 82, 45], ['正木時忠', 68, 50], ['里見義弘', 70, 60],
        \          ['里見義頼', 58, 62], ['正木憲時', 64, 48], ['正木頼忠', 60, 55], ['土岐為頼', 56, 50]],
        \ '上杉': [['柿崎景家', 85, 40], ['直江景綱', 60, 80], ['宇佐美定満', 72, 75],
        \          ['本庄繁長', 84, 45], ['色部勝長', 70, 55], ['斎藤朝信', 75, 68], ['甘粕景持', 74, 50]],
        \ '本願寺': [['下間頼廉', 80, 60], ['七里頼周', 65, 55], ['下間頼照', 60, 58],
        \          ['下間頼旦', 58, 50], ['杉浦玄任', 68, 52], ['願証寺証恵', 50, 60], ['下間光頼', 52, 62]],
        \ '朝倉': [['朝倉宗滴', 88, 70], ['真柄直隆', 80, 25], ['山崎吉家', 65, 60],
        \          ['朝倉景鏡', 60, 58], ['朝倉景健', 62, 55], ['魚住景固', 50, 62], ['前波吉継', 45, 58]],
        \ '武田': [['山本勘助', 75, 80], ['馬場信春', 85, 70], ['山県昌景', 90, 55],
        \          ['高坂昌信', 80, 72], ['内藤昌豊', 78, 70], ['武田信繁', 76, 78], ['真田幸隆', 72, 85]],
        \ '今川': [['太原雪斎', 70, 95], ['朝比奈泰能', 68, 65], ['岡部元信', 78, 45],
        \          ['井伊直盛', 62, 50], ['鵜殿長照', 58, 48], ['関口親永', 50, 62], ['朝比奈泰朝', 66, 55]],
        \ '松平': [['本多忠勝', 92, 40], ['酒井忠次', 75, 70], ['石川数正', 60, 75],
        \          ['榊原康政', 88, 60], ['鳥居元忠', 70, 55], ['大久保忠世', 72, 52], ['服部半蔵', 78, 40]],
        \ '織田': [['柴田勝家', 88, 55], ['木下藤吉郎', 75, 90], ['明智光秀', 82, 85],
        \          ['丹羽長秀', 70, 78], ['前田利家', 80, 60], ['佐々成政', 76, 55], ['池田恒興', 68, 58]],
        \ '斎藤': [['竹中半兵衛', 85, 80], ['安藤守就', 65, 55], ['稲葉一鉄', 75, 60],
        \          ['氏家卜全', 68, 58], ['日根野弘就', 70, 48], ['斎藤利三', 72, 60], ['不破光治', 60, 62]],
        \ '北畠': [['鳥屋尾満栄', 55, 65], ['大宮含忍斎', 60, 50], ['藤方朝成', 58, 55],
        \          ['木造具政', 52, 56], ['北畠具房', 40, 50], ['田丸具忠', 54, 52], ['長野具藤', 50, 45]],
        \ '浅井': [['磯野員昌', 82, 45], ['遠藤直経', 75, 60], ['海北綱親', 78, 55],
        \          ['赤尾清綱', 72, 58], ['雨森清貞', 65, 50], ['浅井久政', 40, 55], ['阿閉貞征', 55, 52]],
        \ '三好': [['三好実休', 75, 65], ['十河一存', 85, 40], ['安宅冬康', 65, 70],
        \          ['三好長逸', 68, 62], ['三好政康', 66, 58], ['岩成友通', 64, 60], ['篠原長房', 62, 75]],
        \ '松永': [['松永長頼', 75, 60], ['竹内秀勝', 60, 55], ['海老名家秀', 55, 50],
        \          ['松永久通', 50, 48], ['高山友照', 58, 60], ['結城忠正', 55, 58], ['四手井家保', 52, 50]],
        \ '雑賀': [['雑賀孫市', 92, 35], ['土橋守重', 70, 55], ['的場昌長', 68, 45],
        \          ['佐武義昌', 72, 40], ['岡吉正', 60, 42], ['土橋若大夫', 62, 48], ['太田左近', 65, 58]],
        \ '波多野': [['波多野秀治', 65, 60], ['荒木氏綱', 62, 55], ['籾井教業', 72, 35],
        \          ['赤井直正', 84, 50], ['波多野秀尚', 58, 50], ['波多野宗長', 55, 48], ['香西元成', 63, 48]],
        \ '赤松': [['別所安治', 62, 55], ['赤松政秀', 58, 50], ['宇野政頼', 55, 52],
        \          ['小寺政職', 45, 58], ['黒田職隆', 60, 68], ['別所就治', 64, 56], ['三木通秋', 52, 50]],
        \ '山名': [['山名豊数', 52, 50], ['垣屋続成', 58, 55], ['太田垣輝延', 55, 52],
        \          ['田結庄是義', 54, 50], ['八木豊信', 53, 51], ['垣屋光成', 56, 54], ['山名豊国', 50, 55]],
        \ '浦上': [['宇喜多直家', 80, 88], ['明石行雄', 62, 58], ['花房正幸', 65, 50],
        \          ['長船貞親', 58, 65], ['戸川秀安', 60, 62], ['岡家利', 57, 55], ['延原景能', 55, 48]],
        \ '尼子': [['山中幸盛', 88, 50], ['立原久綱', 65, 70], ['宇山久兼', 55, 72],
        \          ['亀井秀綱', 52, 64], ['牛尾幸清', 60, 50], ['米原綱寛', 62, 55], ['秋上宗信', 58, 48]],
        \ '毛利': [['吉川元春', 90, 65], ['小早川隆景', 80, 90], ['毛利隆元', 65, 75],
        \          ['福原貞俊', 60, 72], ['口羽通良', 58, 70], ['熊谷信直', 72, 55], ['桂元澄', 62, 64]],
        \ '長宗我部': [['長宗我部元親', 88, 80], ['吉田孝頼', 70, 75], ['福留親政', 75, 45],
        \          ['香宗我部親泰', 68, 66], ['久武昌源', 58, 60], ['吉田重俊', 62, 55], ['谷忠澄', 50, 70]],
        \ '河野': [['村上武吉', 85, 55], ['平岡房実', 55, 65], ['大野直之', 60, 45],
        \          ['村上通康', 75, 58], ['土居清良', 70, 60], ['戒能通森', 56, 48], ['河野通直', 50, 56]],
        \ '大友': [['戸次鑑連', 95, 75], ['臼杵鑑速', 65, 75], ['吉弘鑑理', 70, 65],
        \          ['高橋鑑種', 68, 60], ['志賀親守', 56, 58], ['田原親賢', 60, 62], ['佐伯惟教', 66, 54]],
        \ '龍造寺': [['鍋島直茂', 85, 85], ['成松信勝', 80, 40], ['百武賢兼', 78, 38],
        \          ['木下昌直', 72, 36], ['江里口信常', 74, 35], ['円城寺信胤', 70, 37], ['龍造寺長信', 58, 60]],
        \ '伊東': [['伊東祐安', 60, 50], ['落合兼朝', 55, 52], ['川崎祐長', 58, 48],
        \          ['山田宗昌', 62, 55], ['伊東義益', 50, 58], ['長倉祐政', 54, 46], ['米良矩重', 52, 44]],
        \ '島津': [['島津義久', 75, 88], ['島津義弘', 96, 60], ['島津歳久', 72, 75],
        \          ['島津家久', 90, 55], ['新納忠元', 80, 58], ['伊集院忠朗', 64, 70], ['川上久朗', 66, 56]],
        \ }
endfunc

" ---------------------------------------------------------------------
" データ: 名物茶器 [名前, 値段, 文化値] / 家ごとの初期文化・初期所有
" ---------------------------------------------------------------------
func! s:Items() abort
  return [
        \ {'name':'九十九髪茄子', 'price':3000, 'cul':15},
        \ {'name':'平蜘蛛釜',     'price':2500, 'cul':12},
        \ {'name':'初花肩衝',     'price':2000, 'cul':10},
        \ {'name':'新田肩衝',     'price':2000, 'cul':10},
        \ {'name':'楢柴肩衝',     'price':1800, 'cul':9},
        \ {'name':'三日月茶壷',   'price':1500, 'cul':8},
        \ {'name':'松島の壷',     'price':1200, 'cul':7},
        \ {'name':'珠光小茄子',   'price':1000, 'cul':6},
        \ {'name':'曜変天目',     'price':800,  'cul':5},
        \ {'name':'青磁千声',     'price':600,  'cul':4},
        \ ]
endfunc

func! s:CultureBase() abort
  " 京・駿府・公家・南蛮文化の素地
  return {'今川': 25, '三好': 20, '松永': 20, '北畠': 15, '大友': 15}
endfunc

func! s:InitItemOwners() abort
  " 家名 → 茶器indexリスト (史実風味)
  return {'松永': [0, 1], '三好': [5], '今川': [6]}
endfunc

" ---------------------------------------------------------------------
" 乱数・ユーティリティ
" ---------------------------------------------------------------------
func! s:Rand(n) abort
  if a:n <= 0 | return 0 | endif
  if exists('*rand')
    return rand() % a:n
  endif
  let x = str2nr(matchstr(reltimestr(reltime()), '\.\zs\d\+')[2:7])
  return x % a:n
endfunc

func! s:PadR(str, w) abort
  let pad = a:w - strdisplaywidth(a:str)
  return a:str . (pad > 0 ? repeat(' ', pad) : '')
endfunc

func! s:Log(msg) abort
  call add(s:log, printf('%d年%2d月 %s', s:year, s:month, a:msg))
  if len(s:log) > 300
    let s:log = s:log[-300 :]
  endif
endfunc

func! s:PIdx(name) abort
  for i in range(len(s:prov))
    if s:prov[i].name ==# a:name | return i | endif
  endfor
  return -1
endfunc

func! s:CIdx(name) abort
  for i in range(len(s:clans))
    if s:clans[i].name ==# a:name | return i | endif
  endfor
  return -1
endfunc

func! s:ClanProvs(ci) abort
  let r = []
  for i in range(len(s:prov))
    if s:prov[i].owner == a:ci | call add(r, i) | endif
  endfor
  return r
endfunc

func! s:ClanAlive(ci) abort
  return !empty(s:ClanProvs(a:ci))
endfunc

" ---------------------------------------------------------------------
" 同盟・茶器・文化度
" ---------------------------------------------------------------------
func! s:AllyKey(a, b) abort
  return min([a:a, a:b]) . '-' . max([a:a, a:b])
endfunc

func! s:IsAlly(a, b) abort
  if a:a < 0 || a:b < 0 || a:a == a:b | return 0 | endif
  return has_key(s:ally, s:AllyKey(a:a, a:b))
endfunc

func! s:SetAlly(a, b, on) abort
  let key = s:AllyKey(a:a, a:b)
  if a:on
    let s:ally[key] = 1
  elseif has_key(s:ally, key)
    call remove(s:ally, key)
  endif
endfunc

func! s:AllyList(ci) abort
  let r = []
  for i in range(len(s:clans))
    if i != a:ci && s:IsAlly(a:ci, i) && s:ClanAlive(i)
      call add(r, i)
    endif
  endfor
  return r
endfunc

func! s:DropAlliances(ci) abort
  for i in range(len(s:clans))
    call s:SetAlly(a:ci, i, 0)
  endfor
endfunc

func! s:Culture(ci) abort
  return a:ci >= 0 ? s:cstate[a:ci].cul : 0
endfunc

func! s:ItemOwner(ii) abort
  for ci in range(len(s:clans))
    if index(s:cstate[ci].items, a:ii) >= 0 | return ci | endif
  endfor
  return -1
endfunc

func! s:GainItem(ci, ii) abort
  call add(s:cstate[a:ci].items, a:ii)
  let s:cstate[a:ci].cul = min([100, s:cstate[a:ci].cul + s:items[a:ii].cul])
endfunc

func! s:NeighborClans(ci) abort
  " 領土が接している大名家の一覧
  let seen = {}
  for pi in s:ClanProvs(a:ci)
    for t in s:prov[pi].adj
      let o = s:prov[t].owner
      if o >= 0 && o != a:ci
        let seen[o] = 1
      endif
    endfor
  endfor
  return map(keys(seen), 'str2nr(v:val)')
endfunc

func! s:RichestProv(ci) abort
  " 最も金を持つ自国 (茶器購入・外交費の支払い元)
  let best = -1 | let bg = -1
  for pi in s:ClanProvs(a:ci)
    if s:prov[pi].gold > bg
      let bg = s:prov[pi].gold | let best = pi
    endif
  endfor
  return best
endfunc

" ---------------------------------------------------------------------
" 初期化
" ---------------------------------------------------------------------
func! s:InitState() abort
  let s:clans = s:Clans()
  let s:prov = []
  for d in s:ProvDefs()
    let ex = d[8]
    call add(s:prov, {
          \ 'name': d[0], 'owner': s:CIdx(d[1]), 'gr': d[2], 'gc': d[3],
          \ 'adjn': d[4],
          \ 'koku': d[5], 'comm': d[6], 'sol': d[7],
          \ 'loyal': get(ex, 'loyal', 55 + s:Rand(11)),
          \ 'train': 40 + s:Rand(21),
          \ 'guns': get(ex, 'guns', s:Rand(3) * 10),
          \ 'cannon': 0, 'port': get(ex, 'port', 0),
          \ 'gold': 350 + s:Rand(101) + get(g:, 'sengoku_debug_gold', 0),
          \ 'rice': 900 + s:Rand(201),
          \ 'auto': 0,
          \ })
  endfor
  " 隣接を名前→indexへ解決
  for p in s:prov
    let p.adj = map(copy(p.adjn), 's:PIdx(v:val)')
  endfor
  " 家ごとの状態 (文化度・茶器) と同盟
  let s:items = s:Items()
  let s:cstate = []
  let base = s:CultureBase()
  for c in s:clans
    call add(s:cstate, {'cul': get(base, c.name, 5), 'items': [], 'rank': 0})
  endfor
  for [cname, ilist] in items(s:InitItemOwners())
    for ii in ilist
      call s:GainItem(s:CIdx(cname), ii)
    endfor
  endfor
  let s:ally = {}
  " 武将 (当主 + 家臣3名)
  let s:gens = []
  let gdefs = s:GenDefs()
  for c in s:clans
    let lst = [{'name': c.daimyo, 'war': c.war, 'pol': c.pol}]
    for e in get(gdefs, c.name, [])
      call add(lst, {'name': e[0], 'war': e[1], 'pol': e[2]})
    endfor
    call add(s:gens, lst)
  endfor
  call s:ResetGens()
  let s:year = 1560
  let s:month = 3
  let s:player = -1
endfunc

" ---------------------------------------------------------------------
" 武将 (毎月1人1回行動)
" ---------------------------------------------------------------------
func! s:ResetGens() abort
  let s:gused = []
  for ci in range(len(s:clans))
    call add(s:gused, repeat([0], len(s:gens[ci])))
  endfor
endfunc

func! s:UnusedGens(ci) abort
  return filter(range(len(s:gens[a:ci])), '!s:gused[a:ci][v:val]')
endfunc

func! s:PickGeneral(ci, title) abort
  " 未行動の武将を選ぶ (戻り値: 武将index / -1)
  let cands = s:UnusedGens(a:ci)
  if empty(cands)
    call s:Pause('この月に動ける武将がもういません。')
    return -1
  endif
  let items = map(copy(cands),
        \ 'printf("%s (戦%d 政%d)", s:gens[a:ci][v:val].name,'
        \ . ' s:gens[a:ci][v:val].war, s:gens[a:ci][v:val].pol)')
  let sel = s:Menu(a:title, items)
  if sel < 0 | return -1 | endif
  return cands[sel]
endfunc

func! s:AIBestGen(ci, key) abort
  " 未行動の中から能力値最大の武将 (戻り値: index / -1)
  let best = -1
  let bv = -1
  for gi in s:UnusedGens(a:ci)
    if s:gens[a:ci][gi][a:key] > bv
      let bv = s:gens[a:ci][gi][a:key]
      let best = gi
    endif
  endfor
  return best
endfunc

func! s:BestWarGen(ci) abort
  " 在城武将の最強者 (守城指揮官)
  if a:ci < 0
    return {'name': '足軽大将', 'war': 50, 'pol': 30}
  endif
  let best = s:gens[a:ci][0]
  for g in s:gens[a:ci]
    if g.war > best.war | let best = g | endif
  endfor
  return best
endfunc

func! s:ValidateMap() abort
  " 隣接の対称性・グリッド衝突・名前解決を検査 (戻り値: エラーリスト)
  let errs = []
  let cells = {}
  for i in range(len(s:prov))
    let p = s:prov[i]
    let key = p.gr . ',' . p.gc
    if has_key(cells, key)
      call add(errs, printf('グリッド衝突: %s と %s (%s)', s:prov[cells[key]].name, p.name, key))
    endif
    let cells[key] = i
    for j in p.adj
      if j < 0
        call add(errs, printf('%s: 未解決の隣接名', p.name))
      elseif index(s:prov[j].adj, i) < 0
        call add(errs, printf('隣接が非対称: %s→%s', p.name, s:prov[j].name))
      endif
    endfor
  endfor
  return errs
endfunc

" ---------------------------------------------------------------------
" 画面
" ---------------------------------------------------------------------
func! s:OpenBuf() abort
  if !exists('s:save_ch')
    let s:save_ch = &cmdheight
  endif
  set cmdheight=3
  if exists('s:buf') && bufexists(s:buf)
    let win = bufwinnr(s:buf)
    if win > 0
      execute win . 'wincmd w'
    else
      execute 'silent buffer ' . s:buf
    endif
    return
  endif
  silent tabnew
  silent file 戦国風雲録
  let s:buf = bufnr('%')
  setlocal buftype=nofile bufhidden=hide noswapfile
  setlocal nonumber norelativenumber nolist nowrap
  setlocal nocursorline signcolumn=no
  call s:SetupSyntax()
  " ウィンドウ/フォントサイズ変更時は即座に折り返しを組み直す
  augroup SengokuResize
    autocmd! * <buffer>
    autocmd VimResized <buffer> call s:OnResize()
  augroup END
endfunc

func! s:OnResize() abort
  if get(s:, 'ui_mode', '') ==# 'map'
    call s:Render(get(s:, 'last_active', -1))
    redraw
  endif
endfunc

func! s:CloseUI() abort
  call s:ClosePopups()
  if exists('s:save_ch')
    execute 'set cmdheight=' . s:save_ch
    unlet s:save_ch
  endif
endfunc

func! s:SetupSyntax() abort
  syntax clear
  let colors = ['196', '46', '226', '51', '213', '208', '39', '141',
        \       '254', '118', '203', '75', '228', '99', '214', '159']
  let guis   = ['#ff0000', '#00ff00', '#ffff00', '#00ffff', '#ff87ff',
        \       '#ff8700', '#00afff', '#af87ff', '#e4e4e4', '#87ff00',
        \       '#ff5f5f', '#5fafff', '#ffff87', '#875fff', '#ffaf00',
        \       '#afffff']
  for i in range(len(s:clans))
    let c = i % len(colors)
    execute printf('syntax match SengokuClan%d /%s/', i, s:clans[i].abbr)
    execute printf('highlight SengokuClan%d ctermfg=%s guifg=%s',
          \ i, colors[c], guis[c])
  endfor
  syntax match SengokuTitle /^■.*/
  highlight link SengokuTitle Title
  syntax match SengokuMark />/ contained
  syntax match SengokuActive /|>[^|]*|/ contains=SengokuMark
  highlight SengokuMark ctermfg=231 ctermbg=88 guifg=#ffffff guibg=#870000
  " 籠城戦マップ
  syntax match SengokuAtkU /[１２３４５]/
  syntax match SengokuDefU /[甲乙丙丁戊]/
  syntax match SengokuWallS /壁/
  syntax match SengokuWoodS /森/
  syntax match SengokuGateS /門/
  syntax match SengokuKeepS /本/
  highlight SengokuAtkU ctermfg=203 guifg=#ff5f5f
  highlight SengokuDefU ctermfg=81 guifg=#5fd7ff
  highlight SengokuWallS ctermfg=245 guifg=#8a8a8a
  highlight SengokuWoodS ctermfg=34 guifg=#00af00
  highlight SengokuGateS ctermfg=208 guifg=#ff8700
  highlight SengokuKeepS ctermfg=220 guifg=#ffd700
endfunc

func! s:BoxLines(pi, active) abort
  let p = s:prov[a:pi]
  let inner = s:CELLW - 2
  let title = printf('%d:%s', a:pi + 1, p.name)
  let l1 = '+' . title . repeat('-', inner - strdisplaywidth(title)) . '+'
  let owner = p.owner >= 0 ? s:clans[p.owner].abbr : '--'
  let mark = a:active ==# a:pi ? '>' : (p.owner == s:player && p.auto ? '*' : ' ')
  let sol = printf('%d', (p.sol + 50) / 100)
  let body = mark . owner
  let l2 = '|' . body
        \ . repeat(' ', inner - strdisplaywidth(body) - strdisplaywidth(sol))
        \ . sol . '|'
  let l3 = '+' . repeat('-', inner) . '+'
  return [l1, l2, l3]
endfunc

func! s:PowerTags() abort
  " 生存大名を国数降順のタグ列にする
  let alive = []
  for i in range(len(s:clans))
    let n = len(s:ClanProvs(i))
    if n > 0
      call add(alive, {'i': i, 'n': n})
    endif
  endfor
  call sort(alive, {a, b -> b.n - a.n})
  let tags = []
  for e in alive
    let tag = printf('%s%d', s:clans[e.i].abbr, e.n)
    if e.i == s:player | let tag = '[' . tag . ']' | endif
    call add(tags, tag)
  endfor
  return tags
endfunc

func! s:PowerContent(inner) abort
  " タグ列を幅innerで折り返した行リスト
  let lines = []
  let cur = ''
  for tag in s:PowerTags()
    if !empty(cur) && strdisplaywidth(cur . ' ' . tag) > a:inner
      call add(lines, cur)
      let cur = ''
    endif
    let cur = empty(cur) ? tag : cur . ' ' . tag
  endfor
  if !empty(cur)
    call add(lines, cur)
  endif
  return lines
endfunc

func! s:TruncW(str, w) abort
  let r = a:str
  while strdisplaywidth(r) > a:w
    let r = strcharpart(r, 0, strchars(r) - 1)
  endwhile
  return r
endfunc

func! s:UseFloat() abort
  return has('nvim') && exists('*nvim_open_win')
endfunc

func! s:ClosePopups() abort
  if exists('s:popups')
    for st in values(s:popups)
      if s:UseFloat() && get(st, 'win', -1) != -1 && nvim_win_is_valid(st.win)
        silent! call nvim_win_close(st.win, 1)
      endif
      let st.win = -1
    endfor
  endif
endfunc

func! s:ShowPopup(key, row, col, content, width, title) abort
  " マップに重ねるフローティングウィンドウを作成/更新する (Neovim)
  if !s:UseFloat()
    return
  endif
  if !exists('s:popups')
    let s:popups = {}
  endif
  let st = get(s:popups, a:key, {})
  if empty(st) || !bufexists(get(st, 'buf', -1))
    let st = {'buf': nvim_create_buf(v:false, v:true), 'win': -1}
  endif
  let disp = map(copy(a:content), 's:TruncW(" " . v:val, a:width - 1)')
  call nvim_buf_set_lines(st.buf, 0, -1, v:false, disp)
  let h = max([1, len(disp)])
  " 画面内に収まる位置へクランプ (フォントが大きくても必ず見える)
  let row = max([0, min([a:row, winheight(0) - h - 2])])
  let cfg = {'relative': 'win', 'win': win_getid(), 'row': row, 'col': a:col,
        \ 'width': a:width, 'height': h, 'style': 'minimal',
        \ 'border': 'single', 'title': a:title, 'focusable': v:false, 'zindex': 60}
  try
    if st.win != -1 && nvim_win_is_valid(st.win)
      call nvim_win_set_config(st.win, cfg)
    else
      let st.win = nvim_open_win(st.buf, v:false, cfg)
      call setwinvar(st.win, '&winhighlight', 'NormalFloat:Normal,FloatBorder:Title')
    endif
  catch
    silent! call remove(cfg, 'title')
    let st.win = nvim_open_win(st.buf, v:false, cfg)
  endtry
  let s:popups[a:key] = st
endfunc

func! s:Render(active) abort
  let s:ui_mode = 'map'
  let s:last_active = a:active
  let lines = []
  call add(lines, printf('■ 戦国風雲録  %d年%2d月  〜Vimで天下統一〜  (全%d国)',
        \ s:year, s:month, len(s:prov)))
  if s:player >= 0
    let inames = map(copy(s:cstate[s:player].items), 's:items[v:val].name')
    let anames = map(s:AllyList(s:player), 's:clans[v:val].name')
    let part1 = printf('自家: 官位[%s] 文化%d',
          \ s:RANKS[s:cstate[s:player].rank], s:Culture(s:player))
    let part2 = printf('茶器[%s] 同盟[%s]',
          \ empty(inames) ? 'なし' : join(inames, '・'),
          \ empty(anames) ? 'なし' : join(anames, '・'))
    let limit = max([24, min([s:CELLW * s:GRIDC, winwidth(0) - 1])])
    if strdisplaywidth(part1 . ' ' . part2) > limit
      call add(lines, part1)
      call add(lines, '      ' . part2)
    else
      call add(lines, part1 . ' ' . part2)
    endif
  endif
  call add(lines, '')
  let grid = {}
  for i in range(len(s:prov))
    let grid[s:prov[i].gr . ',' . s:prov[i].gc] = i
  endfor
  for gr in range(s:GRIDR)
    for sub in range(3)
      let line = ''
      for gc in range(s:GRIDC)
        let key = gr . ',' . gc
        if has_key(grid, key)
          let line .= s:BoxLines(grid[key], a:active)[sub]
        else
          let line .= repeat(' ', s:CELLW)
        endif
      endfor
      call add(lines, line)
    endfor
  endfor
  call add(lines, '(数字=兵数/百人  >=行動中  *=委任中  隣接は出陣メニュー参照)')
  if !s:UseFloat()
    " フロート非対応環境: 枠付きパネルとログをバッファ内に描画
    let inner = max([24, min([s:CELLW * s:GRIDC - 2, winwidth(0) - 6])])
    let title = '-- 勢力 '
    call add(lines, '+' . title . repeat('-', inner - strdisplaywidth(title) + 1) . '+')
    for pl in s:PowerContent(inner)
      call add(lines, '|' . s:PadR(' ' . pl, inner + 1) . '|')
    endfor
    call add(lines, '+' . repeat('-', inner + 1) . '+')
    call add(lines, repeat('-', s:CELLW * s:GRIDC))
    let tail = len(s:log) > s:LOGN ? s:log[-s:LOGN :] : s:log
    for m in tail
      call add(lines, m)
    endfor
  endif
  setlocal modifiable
  silent %delete _
  call setline(1, lines)
  setlocal nomodifiable
  if s:UseFloat()
    " 勢力: マップ左上の海域に重ねて表示
    let pw = max([24, min([34, winwidth(0) - 4])])
    call s:ShowPopup('power', 2, 1, s:PowerContent(pw - 3), pw, ' 勢力 ')
    " 戦況: 自家の行動結果や天下の出来事を画面下部に重ねて表示
    let lw = max([24, min([s:CELLW * s:GRIDC - 2, winwidth(0) - 4])])
    let tail = len(s:log) > 5 ? s:log[-5 :] : copy(s:log)
    if empty(tail)
      let tail = ['(まだ出来事はない)']
    endif
    call s:ShowPopup('log', winheight(0) - len(tail) - 2, 1, tail, lw, ' 戦況 ')
  endif
endfunc

func! s:Pause(msg) abort
  call s:Render(-1)
  redraw
  echo a:msg . '  <何かキーで続行>'
  call getchar()
  redraw
endfunc

func! s:Menu(title, items) abort
  let disp = [a:title]
  for i in range(len(a:items))
    call add(disp, printf(' %d. %s', i + 1, a:items[i]))
  endfor
  redraw
  let c = inputlist(disp)
  if c >= 1 && c <= len(a:items)
    return c - 1
  endif
  return -1
endfunc

func! s:AskNum(prompt, max) abort
  redraw
  let s = input(printf('%s (最大%d, 空欄=中止): ', a:prompt, a:max))
  if s !~# '^\d\+$' | return -1 | endif
  let n = str2nr(s)
  if n <= 0 || n > a:max | return -1 | endif
  return n
endfunc

" ---------------------------------------------------------------------
" 戦闘 (共通)
" ---------------------------------------------------------------------
func! s:ConquerApply(aclan, to, sol, guns, train, lines) abort
  " 国を奪取し、滅亡処理(茶器接収・同盟消滅)まで行う
  let t = s:prov[a:to]
  let dclan = t.owner
  let t.owner = a:aclan
  let t.sol = a:sol
  let t.guns = a:guns
  let t.loyal = max([10, t.loyal - 20])
  let t.train = a:train
  let t.auto = 0
  if dclan >= 0 && !s:ClanAlive(dclan)
    call add(a:lines, printf('◆◆ %s家は滅亡した…… ◆◆', s:clans[dclan].name))
    if !empty(s:cstate[dclan].items)
      let got = map(copy(s:cstate[dclan].items), 's:items[v:val].name')
      for ii in s:cstate[dclan].items
        call s:GainItem(a:aclan, ii)
      endfor
      let s:cstate[dclan].items = []
      call add(a:lines, printf('%s家は名物「%s」を接収した!',
            \ s:clans[a:aclan].name, join(got, '」「')))
    endif
    call s:DropAlliances(dclan)
  endif
endfunc

func! s:Battle(aclan, from, to, n, ...) abort
  " ディスパッチャ: プレイヤーが関与する戦いは籠城戦(采配)を選べる
  " a:1 = プレイヤー出陣フラグ, a:2 = 攻手大将(武将dict)
  let atkui = a:0 > 0 && a:1
  let agen = a:0 >= 2 ? a:2 : s:gens[a:aclan][0]
  let f = s:prov[a:from]
  let t = s:prov[a:to]
  let dclan = t.owner
  let aname = s:clans[a:aclan].name
  let dname = dclan >= 0 ? s:clans[dclan].name : '無主'
  let lines = []
  call add(lines, printf('◆ %s軍%d(大将:%s)、%sより%s(%s領)へ侵攻!',
        \ aname, a:n, agen.name, f.name, t.name, dname))
  " 出兵コスト (大筒は1門につき兵1500の人手が要る)
  let aguns = f.sol > 0 ? f.guns * a:n / f.sol : 0
  let s:acan = min([f.cannon, a:n / 1500])
  let atrain = f.train
  let f.sol -= a:n
  let f.guns -= aguns
  let f.cannon -= s:acan
  let f.rice = max([0, f.rice - a:n / 20])
  if t.sol <= 0
    call add(lines, printf('守兵なし! %sは無血開城した。', t.name))
    call s:ConquerApply(a:aclan, a:to, a:n, t.guns + aguns, atrain, lines)
    let t.cannon += s:acan
    return {'win': 1, 'lines': lines}
  endif
  let siege = 0
  if s:player >= 0
    if atkui
      redraw
      let siege = input('采配を振るいますか? (y=籠城戦/n=自動解決): ') =~? '^y'
    elseif dclan == s:player && !t.auto
      call s:Render(-1)
      redraw
      let siege = input(printf('%s軍%d が %s に攻め寄せた! 自ら采配を振るいますか? (y=籠城戦/n=委任): ',
            \ aname, a:n, t.name)) =~? '^y'
    endif
  endif
  if siege
    return s:SiegeBattle(a:aclan, a:from, a:to, a:n, aguns, atrain, agen, lines)
  endif
  return s:AutoBattle(a:aclan, a:from, a:to, a:n, aguns, atrain, agen, lines)
endfunc

func! s:AutoBattle(aclan, from, to, n, aguns, atrain, agen, lines) abort
  let f = s:prov[a:from]
  let t = s:prov[a:to]
  let dclan = t.owner
  let aname = s:clans[a:aclan].name
  let dname = dclan >= 0 ? s:clans[dclan].name : '無主'
  let awar = a:agen.war
  let dwar = s:BestWarGen(dclan).war
  let asol = a:n
  let aguns = a:aguns
  let dsol = t.sol
  let dguns = t.guns
  let round = 0
  " 兵が初期の1/4を切ったら攻め手は自動撤退
  while round < 8 && asol > a:n / 4 && dsol > 0
    let round += 1
    let ap = asol * (100 + a:atrain) / 100 * (80 + awar) / 100 + aguns * 30
          \ + s:acan * 300
    let dp = dsol * (100 + t.train) / 100 * (80 + dwar) / 100 + dguns * 30
          \ + t.cannon * 300
    let dp = dp * 120 / 100
    let dl = ap * (80 + s:Rand(41)) / 100 / 15
    let al = dp * (80 + s:Rand(41)) / 100 / 15
    let dsol = max([0, dsol - dl])
    let asol = max([0, asol - al])
    call add(a:lines, printf('  %d合目: %s軍%d ⇔ %s軍%d', round, aname, asol, dname, dsol))
  endwhile
  if dsol <= 0 && asol > 0
    let lossr = a:n > 0 ? (a:n - asol) * 100 / a:n : 0
    call add(a:lines, printf('★ %s軍勝利! %sを攻略した!', aname, t.name))
    call s:ConquerApply(a:aclan, a:to,
          \ asol, t.guns / 2 + aguns * (100 - lossr) / 100, a:atrain, a:lines)
    let s:prov[a:to].cannon += s:acan
    return {'win': 1, 'lines': a:lines}
  else
    let t.sol = max([dsol, 100])
    let f.sol += asol
    let f.guns += asol > 0 ? aguns : 0
    let f.cannon += s:acan
    call add(a:lines, printf('%s軍は攻めきれず撤退。%sの守りは堅かった。', aname, t.name))
    return {'win': 0, 'lines': a:lines}
  endif
endfunc

" ---------------------------------------------------------------------
" 籠城戦 (攻城戦マップ)
" ---------------------------------------------------------------------
let s:BROWS = 9
let s:BCOLS = 12
let s:KEEP = [4, 8]
let s:GATE = [4, 6]
let s:MOVE = 3
let s:MAXDAY = 30

func! s:SiegeInterior(r, c) abort
  return a:r >= 3 && a:r <= 5 && a:c >= 7 && a:c <= 9
endfunc

func! s:SiegeInit(aclan, from, to, n, aguns, agen) abort
  let t = s:prov[a:to]
  let dclan = t.owner
  let dgen = s:BestWarGen(dclan)
  " 地形: 平地に城郭(壁・門・本丸)と森
  let grid = []
  for r in range(s:BROWS)
    let row = []
    for c in range(s:BCOLS)
      call add(row, '・')
    endfor
    call add(grid, row)
  endfor
  let shp = {}
  for r in range(2, 6)
    for c in range(6, 10)
      if r == 2 || r == 6 || c == 6 || c == 10
        let grid[r][c] = '壁'
        let shp[r . ',' . c] = 200
      endif
    endfor
  endfor
  let grid[s:GATE[0]][s:GATE[1]] = '門'
  let shp[s:GATE[0] . ',' . s:GATE[1]] = 120
  let grid[s:KEEP[0]][s:KEEP[1]] = '本'
  let planted = 0
  while planted < 7
    let r = s:Rand(s:BROWS)
    let c = 2 + s:Rand(4)
    if grid[r][c] ==# '・'
      let grid[r][c] = '森'
      let planted += 1
    endif
  endwhile
  " 部隊編成 (攻手:最大5隊が西から、守手:最大5隊が城内に)
  let units = []
  let alabels = ['１', '２', '３', '４', '５']
  let dlabels = ['甲', '乙', '丙', '丁', '戊']
  let aspots = [[4, 0], [2, 0], [6, 0], [3, 0], [5, 0]]
  let dspots = [[4, 7], [3, 7], [5, 7], [3, 9], [5, 9]]
  let awar = a:agen.war
  let dwar = dgen.war
  let acan = exists('s:acan') ? s:acan : 0
  let na = min([5, max([1, a:n / 800])])
  for i in range(na)
    call add(units, {'side': 0, 'label': alabels[i],
          \ 'sol': a:n / na + (i == 0 ? a:n % na : 0),
          \ 'guns': a:aguns / na, 'train': s:prov[a:from].train, 'war': awar,
          \ 'cannon': acan / na + (i == 0 ? acan % na : 0),
          \ 'r': aspots[i][0], 'c': aspots[i][1]})
  endfor
  let nd = min([5, max([1, t.sol / 800])])
  for i in range(nd)
    call add(units, {'side': 1, 'label': dlabels[i],
          \ 'sol': t.sol / nd + (i == 0 ? t.sol % nd : 0),
          \ 'guns': t.guns / nd, 'train': t.train, 'war': dwar,
          \ 'cannon': t.cannon / nd + (i == 0 ? t.cannon % nd : 0),
          \ 'r': dspots[i][0], 'c': dspots[i][1]})
  endfor
  let s:bt = {'aclan': a:aclan, 'dclan': dclan, 'from': a:from, 'to': a:to,
        \ 'n': a:n, 'day': 1, 'grid': grid, 'shp': shp, 'units': units,
        \ 'msgs': [], 'result': '',
        \ 'agname': a:agen.name, 'dgname': dgen.name,
        \ 'pside': s:player < 0 ? -2 : (a:aclan == s:player ? 0 : 1)}
endfunc

func! s:BtAlive(side) abort
  return filter(copy(s:bt.units), 'v:val.sol > 0 && v:val.side == ' . a:side)
endfunc

func! s:BtUnitAt(r, c) abort
  for u in s:bt.units
    if u.sol > 0 && u.r == a:r && u.c == a:c | return u | endif
  endfor
  return {}
endfunc

func! s:BtMsg(m) abort
  call add(s:bt.msgs, printf('%2d日目: %s', s:bt.day, a:m))
endfunc

func! s:BtPassable(u, r, c) abort
  if a:r < 0 || a:r >= s:BROWS || a:c < 0 || a:c >= s:BCOLS | return 0 | endif
  let ch = s:bt.grid[a:r][a:c]
  if ch ==# '壁' | return 0 | endif
  if ch ==# '門' && a:u.side == 0 | return 0 | endif
  if !empty(s:BtUnitAt(a:r, a:c)) | return 0 | endif
  return 1
endfunc

func! s:BtMoveCost(r, c) abort
  return s:bt.grid[a:r][a:c] ==# '森' ? 2 : 1
endfunc

func! s:BtDefFactor(tu) abort
  " 森と城内は被ダメージ軽減
  let f = 100
  if s:bt.grid[a:tu.r][a:tu.c] ==# '森' | let f -= 20 | endif
  if a:tu.side == 1 && s:SiegeInterior(a:tu.r, a:tu.c) | let f -= 30 | endif
  return f
endfunc

func! s:BtMelee(u, tu) abort
  let dmg = a:u.sol * (100 + a:u.train) / 100 * (80 + a:u.war) / 100 / 10
        \ + a:u.guns * 2
  let dmg = dmg * s:BtDefFactor(a:tu) / 100 * (80 + s:Rand(41)) / 100
  let cnt = a:tu.sol * (100 + a:tu.train) / 100 * (80 + a:tu.war) / 100 / 15
        \ + a:tu.guns
  let cnt = cnt * (80 + s:Rand(41)) / 100
  let a:tu.sol = max([0, a:tu.sol - dmg])
  let a:u.sol = max([0, a:u.sol - cnt])
  call s:BtMsg(printf('%s隊が%s隊に突撃! (敵-%d/自-%d)', a:u.label, a:tu.label, dmg, cnt))
  if a:tu.sol <= 0 | call s:BtMsg(printf('%s隊 壊滅!', a:tu.label)) | endif
  if a:u.sol <= 0 | call s:BtMsg(printf('%s隊 壊滅!', a:u.label)) | endif
endfunc

func! s:BtFire(u, tu) abort
  let dmg = a:u.guns * 8 + a:u.cannon * 25 + a:u.sol / 100
  let dmg = dmg * s:BtDefFactor(a:tu) / 100 * (80 + s:Rand(41)) / 100
  let a:tu.sol = max([0, a:tu.sol - dmg])
  call s:BtMsg(printf('%s隊が%s隊に%s斉射! (敵-%d)', a:u.label, a:tu.label,
        \ a:u.cannon > 0 ? '大筒・鉄砲' : '鉄砲', dmg))
  if a:tu.sol <= 0 | call s:BtMsg(printf('%s隊 壊滅!', a:tu.label)) | endif
endfunc

func! s:BtStructAttack(u, r, c) abort
  let key = a:r . ',' . a:c
  let dmg = a:u.sol / 40 + a:u.train / 5 + a:u.cannon * 40
  let s:bt.shp[key] -= dmg
  let kind = s:bt.grid[a:r][a:c] ==# '門' ? '城門' : '城壁'
  if s:bt.shp[key] <= 0
    let s:bt.grid[a:r][a:c] = '・'
    call s:BtMsg(printf('%s隊が%sを打ち破った!', a:u.label, kind))
  else
    call s:BtMsg(printf('%s隊が%sに攻めかかる (耐久残%d)', a:u.label, kind, s:bt.shp[key]))
  endif
endfunc

func! s:BtAdjTargets(u) abort
  " 隣接する敵部隊と(攻手のみ)城門・城壁
  let r = []
  for d in [[-1, 0], [1, 0], [0, -1], [0, 1]]
    let nr = a:u.r + d[0]
    let nc = a:u.c + d[1]
    if nr < 0 || nr >= s:BROWS || nc < 0 || nc >= s:BCOLS | continue | endif
    let tu = s:BtUnitAt(nr, nc)
    if !empty(tu) && tu.side != a:u.side
      call add(r, {'kind': 'unit', 'u': tu, 'r': nr, 'c': nc,
            \ 'disp': printf('%s隊 (兵%d)', tu.label, tu.sol)})
      continue
    endif
    let ch = s:bt.grid[nr][nc]
    if a:u.side == 0 && (ch ==# '門' || ch ==# '壁')
      call add(r, {'kind': 'struct', 'r': nr, 'c': nc,
            \ 'disp': printf('%s (耐久%d)', ch ==# '門' ? '城門' : '城壁',
            \                s:bt.shp[nr . ',' . nc])})
    endif
  endfor
  return r
endfunc

func! s:BtFireTargets(u) abort
  " 射程2以内の敵部隊
  let r = []
  for tu in s:BtAlive(1 - a:u.side)
    if abs(tu.r - a:u.r) <= 2 && abs(tu.c - a:u.c) <= 2
      call add(r, tu)
    endif
  endfor
  return r
endfunc

func! s:SiegeRender(active) abort
  let s:ui_mode = 'siege'
  call s:ClosePopups()
  let t = s:prov[s:bt.to]
  let lines = []
  call add(lines, printf('■ %s城 攻防戦  %d日目/%d日', t.name, s:bt.day, s:MAXDAY))
  let asum = 0
  let dsum = 0
  for u in s:bt.units
    if u.sol > 0
      if u.side == 0 | let asum += u.sol | else | let dsum += u.sol | endif
    endif
  endfor
  call add(lines, printf('攻手:%s軍(大将:%s) 兵%d%s  守手:%s軍(守将:%s) 兵%d%s',
        \ s:clans[s:bt.aclan].name, s:bt.agname, asum, s:bt.pside == 0 ? '★' : '',
        \ s:bt.dclan >= 0 ? s:clans[s:bt.dclan].name : '無主', s:bt.dgname, dsum,
        \ s:bt.pside == 1 ? '★' : ''))
  call add(lines, '')
  let aus = filter(copy(s:bt.units), 'v:val.side == 0')
  let dus = filter(copy(s:bt.units), 'v:val.side == 1')
  for r in range(s:BROWS)
    let cells = ''
    for c in range(s:BCOLS)
      let u = s:BtUnitAt(r, c)
      let cells .= empty(u) ? s:bt.grid[r][c] : u.label
    endfor
    let mline = s:PadR('  ' . cells, 30)
    if r >= 1 && r <= 5
      let i = r - 1
      if i < len(aus)
        let u = aus[i]
        let mline .= s:PadR(printf('攻%s 兵%d 砲%d 筒%d%s', u.label, u.sol, u.guns, u.cannon,
              \ u.sol <= 0 ? '【壊滅】' : (!empty(a:active) && u is a:active ? '←' : '')), 24)
      else
        let mline .= repeat(' ', 24)
      endif
      if i < len(dus)
        let u = dus[i]
        let mline .= printf('守%s 兵%d 砲%d 筒%d%s', u.label, u.sol, u.guns, u.cannon,
              \ u.sol <= 0 ? '【壊滅】' : (!empty(a:active) && u is a:active ? '←' : ''))
      endif
    endif
    call add(lines, mline)
  endfor
  call add(lines, '')
  call add(lines, '・:平地 森:森(守+) 壁:城壁 門:城門 本:本丸 | １-５:攻手 甲-戊:守手')
  let tail = len(s:bt.msgs) > 4 ? s:bt.msgs[-4 :] : s:bt.msgs
  for m in tail
    call add(lines, m)
  endfor
  setlocal modifiable
  silent %delete _
  call setline(1, lines)
  setlocal nomodifiable
  if !empty(a:active)
    call cursor(4 + a:active.r, 2 + a:active.c * 3 + 1)
  endif
endfunc

func! s:SiegePlayerUnit(u) abort
  let left = s:MOVE
  while s:bt.result ==# '' && a:u.sol > 0
    call s:SiegeRender(a:u)
    redraw
    echo printf('【%s隊】兵%d 砲%d 筒%d 移動残%d | hjkl:移動 a:攻撃 f:射撃(射程2) w:待機 Q:%s',
          \ a:u.label, a:u.sol, a:u.guns, a:u.cannon, left,
          \ s:bt.pside == 0 ? '全軍退却' : '開城')
    let raw = getchar()
    let c = type(raw) == type(0) ? nr2char(raw) : raw
    let dr = 0
    let dc = 0
    let mv = 0
    if c ==# 'h' || c ==# "\<Left>"
      let dc = -1 | let mv = 1
    elseif c ==# 'l' || c ==# "\<Right>"
      let dc = 1 | let mv = 1
    elseif c ==# 'k' || c ==# "\<Up>"
      let dr = -1 | let mv = 1
    elseif c ==# 'j' || c ==# "\<Down>"
      let dr = 1 | let mv = 1
    elseif c ==# 'a'
      let tgts = s:BtAdjTargets(a:u)
      if empty(tgts)
        call s:BtMsg('攻撃できる相手が隣接していない')
        continue
      endif
      let sel = 0
      if len(tgts) > 1
        let sel = s:Menu('== 攻撃目標 ==', map(copy(tgts), 'v:val.disp'))
        if sel < 0 | continue | endif
      endif
      if tgts[sel].kind ==# 'unit'
        call s:BtMelee(a:u, tgts[sel].u)
      else
        call s:BtStructAttack(a:u, tgts[sel].r, tgts[sel].c)
      endif
      return
    elseif c ==# 'f'
      if a:u.guns <= 0 && a:u.cannon <= 0
        call s:BtMsg('鉄砲も大筒も持っていない')
        continue
      endif
      let tgts = s:BtFireTargets(a:u)
      if empty(tgts)
        call s:BtMsg('射程(2)内に敵がいない')
        continue
      endif
      let sel = 0
      if len(tgts) > 1
        let sel = s:Menu('== 射撃目標 ==',
              \ map(copy(tgts), 'printf("%s隊 (兵%d)", v:val.label, v:val.sol)'))
        if sel < 0 | continue | endif
      endif
      call s:BtFire(a:u, tgts[sel])
      return
    elseif c ==# 'w' || c ==# ' ' || c ==# "\r"
      return
    elseif c ==# 'Q'
      redraw
      if s:bt.pside == 0
        if input('全軍退却しますか? (yes/no): ') ==# 'yes'
          let s:bt.result = 'retreat'
        endif
      else
        if input('城を明け渡して落ち延びますか? (yes/no): ') ==# 'yes'
          let s:bt.result = 'surrender'
        endif
      endif
    endif
    if mv
      let nr = a:u.r + dr
      let nc = a:u.c + dc
      if !s:BtPassable(a:u, nr, nc) | continue | endif
      let cost = s:BtMoveCost(nr, nc)
      if cost > left
        call s:BtMsg('移動力が足りない')
        continue
      endif
      let left -= cost
      let a:u.r = nr
      let a:u.c = nc
      if a:u.side == 0 && nr == s:KEEP[0] && nc == s:KEEP[1]
        let s:bt.result = 'fall'
        return
      endif
    endif
  endwhile
endfunc

func! s:SiegeAIUnit(u) abort
  let enemies = s:BtAlive(1 - a:u.side)
  if empty(enemies) | return | endif
  " 鉄砲があれば射撃優先
  if a:u.guns > 0 || a:u.cannon > 0
    let ft = s:BtFireTargets(a:u)
    if !empty(ft)
      call s:BtFire(a:u, ft[0])
      return
    endif
  endif
  " 目標決定
  if a:u.side == 0
    let goal = s:bt.grid[s:GATE[0]][s:GATE[1]] ==# '門' ? s:GATE : s:KEEP
  else
    " 守手: 門が無事で敵が遠ければ持ち場を守る
    let threat = 0
    for e in enemies
      if s:SiegeInterior(e.r, e.c) || abs(e.r - a:u.r) + abs(e.c - a:u.c) <= 2
        let threat = 1
      endif
    endfor
    if s:bt.grid[s:GATE[0]][s:GATE[1]] ==# '門' && !threat
      let tgts = s:BtAdjTargets(a:u)
      if !empty(tgts) && tgts[0].kind ==# 'unit'
        call s:BtMelee(a:u, tgts[0].u)
      endif
      return
    endif
    let goal = [enemies[0].r, enemies[0].c]
    for e in enemies
      if abs(e.r - a:u.r) + abs(e.c - a:u.c)
            \ < abs(goal[0] - a:u.r) + abs(goal[1] - a:u.c)
        let goal = [e.r, e.c]
      endif
    endfor
  endif
  let left = s:MOVE
  while left > 0
    " 隣接敵に攻撃
    let tgts = s:BtAdjTargets(a:u)
    for tg in tgts
      if tg.kind ==# 'unit'
        call s:BtMelee(a:u, tg.u)
        return
      endif
    endfor
    " 攻手は城門(なければ城壁)に取りつく
    if a:u.side == 0
      for tg in tgts
        if tg.kind ==# 'struct' && s:bt.grid[tg.r][tg.c] ==# '門'
          call s:BtStructAttack(a:u, tg.r, tg.c)
          return
        endif
      endfor
    endif
    " 一歩前進 (目標へ近づく方向で通行可能なマス)
    let curd = abs(goal[0] - a:u.r) + abs(goal[1] - a:u.c)
    let moved = 0
    for d in [[0, 1], [0, -1], [-1, 0], [1, 0]]
      let nr = a:u.r + d[0]
      let nc = a:u.c + d[1]
      if !s:BtPassable(a:u, nr, nc) | continue | endif
      if abs(goal[0] - nr) + abs(goal[1] - nc) >= curd | continue | endif
      let cost = s:BtMoveCost(nr, nc)
      if cost > left | continue | endif
      let left -= cost
      let a:u.r = nr
      let a:u.c = nc
      let moved = 1
      if a:u.side == 0 && nr == s:KEEP[0] && nc == s:KEEP[1]
        let s:bt.result = 'fall'
        return
      endif
      break
    endfor
    if !moved
      " 進めない攻手は目の前の城壁を崩す
      if a:u.side == 0 && !empty(tgts)
        for tg in tgts
          if tg.kind ==# 'struct'
            call s:BtStructAttack(a:u, tg.r, tg.c)
            return
          endif
        endfor
      endif
      return
    endif
  endwhile
endfunc

func! s:SiegeBattle(aclan, from, to, n, aguns, atrain, agen, lines) abort
  call s:SiegeInit(a:aclan, a:from, a:to, a:n, a:aguns, a:agen)
  let f = s:prov[a:from]
  let t = s:prov[a:to]
  let aname = s:clans[a:aclan].name
  call s:BtMsg(printf('%s城の戦い、火蓋が切られた!', t.name))
  while s:bt.result ==# ''
    for side in [0, 1]
      for u in s:bt.units
        if s:bt.result !=# '' | break | endif
        if u.side != side || u.sol <= 0 | continue | endif
        if side == s:bt.pside
          call s:SiegePlayerUnit(u)
        else
          call s:SiegeAIUnit(u)
        endif
      endfor
      if empty(s:BtAlive(0)) && s:bt.result ==# ''
        let s:bt.result = 'destroyed'
      endif
      if empty(s:BtAlive(1)) && s:bt.result ==# ''
        let s:bt.result = 'annihilated'
      endif
      if s:bt.result !=# '' | break | endif
    endfor
    if s:bt.result ==# ''
      let s:bt.day += 1
      if s:bt.day > s:MAXDAY
        let s:bt.result = 'timeout'
      endif
      " AI攻手は損害が初期の3/4に達すると兵を退く
      if s:bt.pside != 0 && s:bt.result ==# ''
        let asum = 0
        for u in s:BtAlive(0)
          let asum += u.sol
        endfor
        if asum < a:n / 4
          let s:bt.result = 'timeout'
        endif
      endif
    endif
  endwhile
  " 決着
  let asol = 0
  let ag = 0
  let acan2 = 0
  for u in s:BtAlive(0)
    let asol += u.sol
    let ag += u.guns
    let acan2 += u.cannon
  endfor
  let dsol = 0
  let dg = 0
  let dcan2 = 0
  for u in s:BtAlive(1)
    let dsol += u.sol
    let dg += u.guns
    let dcan2 += u.cannon
  endfor
  let res = s:bt.result
  let win = res ==# 'fall' || res ==# 'annihilated' || res ==# 'surrender'
  if win
    if res ==# 'fall'
      call add(a:lines, printf('★ %s軍、本丸を占拠! %s城は落城した!(%d日)',
            \ aname, t.name, s:bt.day))
    elseif res ==# 'surrender'
      call add(a:lines, printf('★ 守将は城を明け渡した。%s城 開城。', t.name))
    else
      call add(a:lines, printf('★ 守兵全滅! %s城は落城した!(%d日)', t.name, s:bt.day))
    endif
    call s:ConquerApply(a:aclan, a:to, max([asol, 100]), ag + dg / 2, a:atrain, a:lines)
    let t.cannon = acan2 + dcan2
  else
    if res ==# 'destroyed'
      call add(a:lines, printf('%s軍は全滅…… %s城は守り抜かれた!', aname, t.name))
    elseif res ==# 'retreat'
      call add(a:lines, printf('%s軍は退却した。', aname))
    else
      call add(a:lines, printf('%s軍は攻めきれず兵を退いた。(%d日)', aname, s:bt.day))
    endif
    let f.sol += asol
    let f.guns += ag
    let f.cannon += acan2
    let t.sol = max([dsol, 100])
    let t.guns = dg
    let t.cannon = dcan2
  endif
  call s:SiegeRender({})
  if s:bt.pside >= 0
    redraw
    echo join(a:lines, "\n") . "\n<何かキーで続行>"
    call getchar()
  endif
  return {'win': win, 'lines': a:lines, 'shown': 1}
endfunc

" ---------------------------------------------------------------------
" AI (委任国でも使用)
" ---------------------------------------------------------------------
func! s:AIProvAct(ci, pi) abort
  " 未行動の武将1人を使って国を差配する (武将が尽きたら何もしない)
  let p = s:prov[a:pi]
  if empty(s:UnusedGens(a:ci)) | return | endif
  " 1) 勝てそうな隣接敵に、最強の武将を大将に立てて出陣
  let best = -1 | let bestsol = 999999
  for t in p.adj
    let tp = s:prov[t]
    if tp.owner == a:ci || s:IsAlly(a:ci, tp.owner) | continue | endif
    if tp.sol * 3 / 2 + 500 < p.sol && tp.sol < bestsol
      let best = t | let bestsol = tp.sol
    endif
  endfor
  if best >= 0 && p.sol > 2500
    let gi = s:AIBestGen(a:ci, 'war')
    let s:gused[a:ci][gi] = 1
    let r = s:Battle(a:ci, a:pi, best, p.sol - 1000, 0, s:gens[a:ci][gi])
    for l in r.lines
      " AI戦は経過を省き結果だけログ
      if l !~# '合目'
        call s:Log(l)
      endif
    endfor
    return
  endif
  " 2) 南蛮交易 (港があれば商人と取引、武将は使わない)
  if p.port && p.gold >= 1000 && s:Rand(100) < 25
    if s:Rand(100) < 40
      let p.gold -= 800
      let p.cannon += 1
    else
      let p.gold -= 500
      let p.guns += 40
    endif
    return
  endif
  " 3) 内政
  let cap = p.koku * 20 - p.sol
  if p.gold >= 400 && cap > 1000 && min([p.gold, p.rice]) * 5 > 1000
    let gi = s:AIBestGen(a:ci, 'pol')
    let s:gused[a:ci][gi] = 1
    let n = min([cap, min([p.gold, p.rice]) * 5 / 2])
    let n = min([n, 3000])
    let p.gold -= n / 5 | let p.rice -= n / 5
    let p.sol += n
    let p.loyal = max([0, p.loyal - n / 500 - 2])
  elseif p.gold >= 300
    let roll = s:Rand(4)
    if roll == 0
      let gi = s:AIBestGen(a:ci, 'pol')
      let s:gused[a:ci][gi] = 1
      let p.gold -= 200
      let p.koku += 12 + s:gens[a:ci][gi].pol / 8
    elseif roll == 1
      let gi = s:AIBestGen(a:ci, 'pol')
      let s:gused[a:ci][gi] = 1
      let p.gold -= 200
      let p.comm += 6 + s:gens[a:ci][gi].pol / 12
    elseif roll == 2 && p.train < 95
      let gi = s:AIBestGen(a:ci, 'war')
      let s:gused[a:ci][gi] = 1
      let p.gold -= 100
      let p.train = min([100, p.train + 5 + s:gens[a:ci][gi].war / 15])
    elseif p.loyal < 50 && p.rice > 300
      let gi = s:AIBestGen(a:ci, 'pol')
      let s:gused[a:ci][gi] = 1
      let p.gold -= 100 | let p.rice -= 100
      let p.loyal = min([100, p.loyal + 10])
    endif
  endif
endfunc

func! s:AITurns() abort
  for ci in range(len(s:clans))
    if ci == s:player | continue | endif
    for pi in s:ClanProvs(ci)
      if s:prov[pi].owner != ci | continue | endif
      if empty(s:UnusedGens(ci)) | break | endif
      call s:AIProvAct(ci, pi)
    endfor
    if s:player >= 0 && !s:ClanAlive(s:player)
      return
    endif
  endfor
endfunc

" ---------------------------------------------------------------------
" プレイヤーのターン
" ---------------------------------------------------------------------
func! s:ShowInfo() abort
  let items = map(range(len(s:prov)),
        \ 'printf("%s (%s)", s:prov[v:val].name, s:prov[v:val].owner >= 0 ? s:clans[s:prov[v:val].owner].name : "--")')
  let sel = s:Menu('== 情報を見る国 ==', items)
  if sel < 0 | return | endif
  let p = s:prov[sel]
  let c = p.owner >= 0 ? s:clans[p.owner] : {}
  redraw
  echo printf('【%s】 領主:%s(%s)%s', p.name,
        \ empty(c) ? '--' : c.name, empty(c) ? '--' : c.daimyo,
        \ p.owner == s:player && p.auto ? ' [委任中]' : '')
  echon printf("\n 石高:%d 商業:%d 民忠:%d%s\n 兵数:%d 訓練:%d 鉄砲:%d 大筒:%d\n 金:%d 米:%d",
        \ p.koku, p.comm, p.loyal, p.port ? ' 【南蛮港】' : '',
        \ p.sol, p.train, p.guns, p.cannon, p.gold, p.rice)
  echon "\n 隣接: " . join(map(copy(p.adj), 's:prov[v:val].name'), '・')
  echon "\n<何かキーで戻る>"
  call getchar()
endfunc

func! s:ShowGenerals() abort
  let lines = ['== 自家の武将 (毎月1人1回行動) ==']
  for gi in range(len(s:gens[s:player]))
    let g = s:gens[s:player][gi]
    call add(lines, printf(' %s %s%s 戦%d 政%d', s:gused[s:player][gi] ? '済' : '　',
          \ g.name, gi == 0 ? '(当主)' : '', g.war, g.pol))
  endfor
  redraw
  echo join(lines, "\n") . "\n<何かキーで戻る>"
  call getchar()
endfunc

func! s:CmdAttack(pi) abort
  let p = s:prov[a:pi]
  let targets = filter(copy(p.adj),
        \ 's:prov[v:val].owner != s:player && !s:IsAlly(s:player, s:prov[v:val].owner)')
  if empty(targets)
    call s:Pause('隣接する敵領がありません(同盟国へは出陣できません)。')
    return 0
  endif
  let items = map(copy(targets),
        \ 'printf("%s (%s領 兵%d)", s:prov[v:val].name, s:clans[s:prov[v:val].owner].name, s:prov[v:val].sol)')
  let sel = s:Menu('== 出陣先 ==', items)
  if sel < 0 | return 0 | endif
  let to = targets[sel]
  let gi = s:PickGeneral(s:player, '== 出陣の大将 (戦才が全部隊に影響) ==')
  if gi < 0 | return 0 | endif
  let n = s:AskNum('出陣する兵数', p.sol)
  if n < 0 | return 0 | endif
  let s:gused[s:player][gi] = 1
  let r = s:Battle(s:player, a:pi, to, n, 1, s:gens[s:player][gi])
  for l in r.lines
    call s:Log(l)
  endfor
  if !get(r, 'shown', 0)
    call s:Render(-1)
    redraw
    echo join(r.lines, "\n") . "\n<何かキーで続行>"
    call getchar()
  endif
  return 1
endfunc

func! s:CmdTransport(pi) abort
  let p = s:prov[a:pi]
  let targets = filter(copy(p.adj), 's:prov[v:val].owner == s:player')
  if empty(targets)
    call s:Pause('隣接する自領がありません。')
    return 0
  endif
  let items = map(copy(targets), 's:prov[v:val].name')
  let sel = s:Menu('== 輸送先 ==', items)
  if sel < 0 | return 0 | endif
  let to = targets[sel]
  let kind = s:Menu('== 何を送る? ==', ['金 (' . p.gold . ')', '米 (' . p.rice . ')', '兵 (' . p.sol . ')'])
  if kind < 0 | return 0 | endif
  let key = ['gold', 'rice', 'sol'][kind]
  let n = s:AskNum('送る量', p[key])
  if n < 0 | return 0 | endif
  let p[key] -= n
  let s:prov[to][key] += n
  if kind == 2
    let g = (p.sol + n) > 0 ? p.guns * n / (p.sol + n) : 0
    let p.guns -= g
    let s:prov[to].guns += g
  endif
  call s:Log(printf('%s→%s %sを%d輸送', p.name, s:prov[to].name, ['金', '米', '兵'][kind], n))
  return 1
endfunc

func! s:CmdRelease() abort
  let autos = filter(s:ClanProvs(s:player), 's:prov[v:val].auto')
  if empty(autos)
    call s:Pause('委任中の国はありません。')
    return
  endif
  let items = map(copy(autos), 's:prov[v:val].name')
  let sel = s:Menu('== 委任を解除する国 ==', items)
  if sel < 0 | return | endif
  let s:prov[autos[sel]].auto = 0
  call s:Log(printf('%s の委任を解除', s:prov[autos[sel]].name))
endfunc

func! s:ClanReport() abort
  " 諸家の状況 (文化度・茶器・同盟)
  let lines = ['== 諸家の状況 ==']
  for ci in range(len(s:clans))
    if !s:ClanAlive(ci) | continue | endif
    let c = s:clans[ci]
    let inames = map(copy(s:cstate[ci].items), 's:items[v:val].name')
    let anames = map(s:AllyList(ci), 's:clans[v:val].name')
    call add(lines, printf('%s%s家 %d国 %s 文化%d 茶器[%s] 同盟[%s]',
          \ ci == s:player ? '*' : ' ', c.name, len(s:ClanProvs(ci)),
          \ s:RANKS[s:cstate[ci].rank], s:Culture(ci),
          \ empty(inames) ? 'なし' : join(inames, '・'),
          \ empty(anames) ? 'なし' : join(anames, '・')))
  endfor
  redraw
  echo join(lines, "\n") . "\n<何かキーで戻る>"
  call getchar()
endfunc

func! s:CmdDiplo(pi) abort
  " 戻り値: 1=コマンド消費 0=キャンセル
  let p = s:prov[a:pi]
  let sel = s:Menu('== 外交 ==',
        \ ['同盟を申し入れる (贈物 金500)', '同盟を破棄する', '諸家の状況を見る'])
  if sel < 0 | return 0 | endif
  if sel == 2
    call s:ClanReport()
    return 0
  endif
  if sel == 0
    if p.gold < 500
      call s:Pause('贈物の金500がこの国にありません。')
      return 0
    endif
    let cands = filter(range(len(s:clans)),
          \ 'v:val != s:player && s:ClanAlive(v:val) && !s:IsAlly(s:player, v:val)')
    if empty(cands)
      call s:Pause('同盟を結べる相手がいません。')
      return 0
    endif
    let items = map(copy(cands),
          \ 'printf("%s家 (%d国)", s:clans[v:val].name, len(s:ClanProvs(v:val)))')
    let t = s:Menu('== 同盟の相手 ==', items)
    if t < 0 | return 0 | endif
    let target = cands[t]
    let gi = s:PickGeneral(s:player, '== 使者に立てる武将 (政才で成功率UP) ==')
    if gi < 0 | return 0 | endif
    let g = s:gens[s:player][gi]
    let s:gused[s:player][gi] = 1
    let p.gold -= 500
    let chance = 25 + g.pol / 3 + s:Culture(s:player) / 5
          \ + len(s:cstate[s:player].items) * 4
          \ + s:cstate[s:player].rank * 3
    let chance = min([90, chance])
    if s:Rand(100) < chance
      call s:SetAlly(s:player, target, 1)
      call s:Log(printf('使者%s、%s家との同盟をまとめた!', g.name, s:clans[target].name))
      call s:Pause(printf('%s家は同盟を快諾した!(成功率%d%%)', s:clans[target].name, chance))
    else
      call s:Log(printf('使者%s、%s家との交渉は決裂……', g.name, s:clans[target].name))
      call s:Pause(printf('%s家は申し出を断った……(成功率%d%%)', s:clans[target].name, chance))
    endif
    return 1
  endif
  " 同盟破棄
  let allies = s:AllyList(s:player)
  if empty(allies)
    call s:Pause('同盟中の家はありません。')
    return 0
  endif
  let items = map(copy(allies), 's:clans[v:val].name . "家"')
  let t = s:Menu('== 破棄する同盟 ==', items)
  if t < 0 | return 0 | endif
  call s:SetAlly(s:player, allies[t], 0)
  let s:cstate[s:player].cul = max([0, s:cstate[s:player].cul - 3])
  call s:Log(printf('%s家との同盟を破棄 (信義を失い文化-3)', s:clans[allies[t]].name))
  return 1
endfunc

func! s:CmdCourt(pi) abort
  " 朝廷: 献金して官位を賜る / 勅命和睦
  let p = s:prov[a:pi]
  let rank = s:cstate[s:player].rank
  let sel = s:Menu(printf('== 朝廷 (現在の官位: %s) ==', s:RANKS[rank]),
        \ ['献金して官位を賜る (金1000)',
        \  '勅命和睦を奏請する (金2000・要官位)'])
  if sel < 0 | return 0 | endif
  if sel == 0
    if rank >= len(s:RANKS) - 1
      call s:Pause('これ以上の官位はありません(すでに' . s:RANKS[rank] . ')。')
      return 0
    endif
    if p.gold < 1000
      call s:Pause('献金の金1000がこの国にありません。')
      return 0
    endif
    let gi = s:PickGeneral(s:player, '== 京へ上らせる使者 (政才で成功率UP) ==')
    if gi < 0 | return 0 | endif
    let g = s:gens[s:player][gi]
    let s:gused[s:player][gi] = 1
    let p.gold -= 1000
    let kyoto = s:PIdx('山城')
    let chance = 40 + g.pol / 3 + s:Culture(s:player) / 5 - rank * 8
          \ + (s:prov[kyoto].owner == s:player ? 30 : 0)
    let chance = max([5, min([95, chance])])
    if s:Rand(100) < chance
      let s:cstate[s:player].rank = rank + 1
      for pi2 in s:ClanProvs(s:player)
        let s:prov[pi2].loyal = min([100, s:prov[pi2].loyal + 8])
      endfor
      call s:Log(printf('使者%s、朝廷より「%s」の官位を賜る! 領民歓喜(民忠+8)',
            \ g.name, s:RANKS[rank + 1]))
      call s:Pause(printf('帝より「%s」に叙せられた!(成功率%d%%)', s:RANKS[rank + 1], chance))
    else
      call s:Log(printf('使者%sの献金むなしく、官位は沙汰やみに……', g.name))
      call s:Pause(printf('官位の沙汰はなかった……(成功率%d%%)', chance))
    endif
    return 1
  endif
  " 勅命和睦
  if rank < 1
    call s:Pause('勅命を奏請するには官位が必要です。まず献金を。')
    return 0
  endif
  if p.gold < 2000
    call s:Pause('奏請の金2000がこの国にありません。')
    return 0
  endif
  let cands = filter(range(len(s:clans)),
        \ 'v:val != s:player && s:ClanAlive(v:val) && !s:IsAlly(s:player, v:val)')
  if empty(cands)
    call s:Pause('和睦を結ぶ相手がいません。')
    return 0
  endif
  let items = map(copy(cands),
        \ 'printf("%s家 (%d国)", s:clans[v:val].name, len(s:ClanProvs(v:val)))')
  let t = s:Menu('== 勅命和睦の相手 ==', items)
  if t < 0 | return 0 | endif
  let gi = s:PickGeneral(s:player, '== 京へ上らせる使者 ==')
  if gi < 0 | return 0 | endif
  let g = s:gens[s:player][gi]
  let s:gused[s:player][gi] = 1
  let p.gold -= 2000
  call s:SetAlly(s:player, cands[t], 1)
  call s:Log(printf('帝の勅命により%s家と和睦(同盟)が成った! (使者:%s)',
        \ s:clans[cands[t]].name, g.name))
  call s:Pause(printf('勅命なれば是非もなし——%s家との同盟が成立した。', s:clans[cands[t]].name))
  return 1
endfunc

func! s:CmdNanban(pi) abort
  " 南蛮交易: 港のある国のみ。文化度が高いほど割引
  let p = s:prov[a:pi]
  if !p.port
    call s:Pause('南蛮船の入る港がありません(堺=摂津・博多=筑前・府内=豊後・平戸=肥前・坊津=薩摩)。')
    return 0
  endif
  let cul = s:Culture(s:player)
  let disc = 100 - cul / 2
  let pgun = 500 * disc / 100
  let pcan = 800 * disc / 100
  let prare = 600 * disc / 100
  let sel = s:Menu(printf('== 南蛮商人 (文化%d 割引%d%%) ==', cul, 100 - disc),
        \ [printf('鉄砲%d挺を買う (金%d)', 40 + cul / 5, pgun),
        \  printf('大筒1門を買う (金%d) ※城攻めの切り札', pcan),
        \  printf('南蛮の珍品を買う (金%d) 文化+5', prare)])
  if sel < 0 | return 0 | endif
  if sel == 0
    if p.gold < pgun | call s:Pause('金が足りません。') | return 0 | endif
    let p.gold -= pgun
    let got = 40 + cul / 5
    let p.guns += got
    call s:Log(printf('%s 南蛮船より鉄砲%d挺を購入 (計%d)', p.name, got, p.guns))
  elseif sel == 1
    if p.gold < pcan | call s:Pause('金が足りません。') | return 0 | endif
    let p.gold -= pcan
    let p.cannon += 1
    call s:Log(printf('%s 南蛮船より大筒1門を購入 (計%d門)', p.name, p.cannon))
  else
    if p.gold < prare | call s:Pause('金が足りません。') | return 0 | endif
    let p.gold -= prare
    let s:cstate[s:player].cul = min([100, s:cstate[s:player].cul + 5])
    let rare = ['ギヤマンの壺', '地球儀', '南蛮時計', 'ビロードのマント', '金平糖'][s:Rand(5)]
    call s:Log(printf('%s 南蛮の珍品「%s」を入手 文化+5 (%d)', p.name, rare, s:Culture(s:player)))
  endif
  return 1
endfunc

func! s:CmdTea(pi) abort
  let p = s:prov[a:pi]
  if empty(s:cstate[s:player].items)
    call s:Pause('茶会には名物茶器が必要です。堺の商人を待ちましょう。')
    return 0
  endif
  if p.gold < 300
    call s:Pause('茶会には金300が必要です。')
    return 0
  endif
  let p.gold -= 300
  let up = 2 + s:clans[s:player].chr / 30
  for pi2 in s:ClanProvs(s:player)
    let s:prov[pi2].loyal = min([100, s:prov[pi2].loyal + up])
  endfor
  let s:cstate[s:player].cul = min([100, s:cstate[s:player].cul + 2])
  call s:Log(printf('盛大な茶会を開催 全領土の民忠+%d 文化+2 (%d)',
        \ up, s:Culture(s:player)))
  return 1
endfunc

func! s:PlayerCommand(pi) abort
  " 戻り値: '' 通常 / 'END' この月の残りを待機
  let p = s:prov[a:pi]
  while 1
    call s:Render(a:pi)
    redraw
    echo printf('【%s】金%d 米%d 兵%d 民忠%d 訓練%d 鉄砲%d | 文化%d 茶器%d 動ける武将%d',
          \ p.name, p.gold, p.rice, p.sol, p.loyal, p.train, p.guns,
          \ s:Culture(s:player), len(s:cstate[s:player].items),
          \ len(s:UnusedGens(s:player)))
    echon "\n1開発 2商業 3施し 4徴兵 5訓練 6鉄砲 7出陣 8輸送 9外交 C朝廷 N南蛮 T茶会 B武将 A委任 R解除 i情報 0待機 E残待機 S保存 Q降参 > "
    let raw = getchar()
    let c = type(raw) == type(0) ? nr2char(raw) : ''
    if c ==# '1'          " 開発
      if p.gold < 200 | call s:Pause('金が足りません(200必要)。') | continue | endif
      let gi = s:PickGeneral(s:player, '== 開発を任せる武将 (政才で効果UP) ==')
      if gi < 0 | continue | endif
      let g = s:gens[s:player][gi]
      let s:gused[s:player][gi] = 1
      let p.gold -= 200
      let up = 12 + g.pol / 8 + s:Rand(6)
      let p.koku += up
      let p.loyal = min([100, p.loyal + 2])
      call s:Log(printf('%s %sが開発 石高+%d (%d)', p.name, g.name, up, p.koku))
      return ''
    elseif c ==# '2'      " 商業
      if p.gold < 200 | call s:Pause('金が足りません(200必要)。') | continue | endif
      let gi = s:PickGeneral(s:player, '== 商業を任せる武将 (政才で効果UP) ==')
      if gi < 0 | continue | endif
      let g = s:gens[s:player][gi]
      let s:gused[s:player][gi] = 1
      let p.gold -= 200
      let up = 6 + g.pol / 12 + s:Rand(4)
      let p.comm += up
      call s:Log(printf('%s %sが商業投資 商業+%d (%d)', p.name, g.name, up, p.comm))
      return ''
    elseif c ==# '3'      " 施し
      if p.gold < 100 || p.rice < 100
        call s:Pause('金100と米100が必要です。') | continue
      endif
      let gi = s:PickGeneral(s:player, '== 施しを任せる武将 (政才で効果UP) ==')
      if gi < 0 | continue | endif
      let g = s:gens[s:player][gi]
      let s:gused[s:player][gi] = 1
      let p.gold -= 100 | let p.rice -= 100
      let up = 8 + g.pol / 15
      let p.loyal = min([100, p.loyal + up])
      call s:Log(printf('%s %sが施し 民忠+%d (%d)', p.name, g.name, up, p.loyal))
      return ''
    elseif c ==# '4'      " 徴兵
      let cap = p.koku * 20 - p.sol
      let afford = min([p.gold * 5, p.rice * 5])
      let maxn = min([cap, afford])
      if maxn < 100 | call s:Pause('これ以上徴兵できません(石高上限か金米不足)。') | continue | endif
      let gi = s:PickGeneral(s:player, '== 徴兵を任せる武将 ==')
      if gi < 0 | continue | endif
      let n = s:AskNum('徴兵数', maxn)
      if n < 0 | continue | endif
      let g = s:gens[s:player][gi]
      let s:gused[s:player][gi] = 1
      let p.gold -= n / 5 | let p.rice -= n / 5
      let p.sol += n
      let p.loyal = max([0, p.loyal - n / 500 - 2])
      let p.train = p.train * p.sol / (p.sol + n / 2)
      call s:Log(printf('%s %sが徴兵+%d (兵%d)', p.name, g.name, n, p.sol))
      return ''
    elseif c ==# '5'      " 訓練
      if p.gold < 100 | call s:Pause('金が足りません(100必要)。') | continue | endif
      if p.train >= 100 | call s:Pause('訓練度は既に最大です。') | continue | endif
      let gi = s:PickGeneral(s:player, '== 訓練を任せる武将 (戦才で効果UP) ==')
      if gi < 0 | continue | endif
      let g = s:gens[s:player][gi]
      let s:gused[s:player][gi] = 1
      let p.gold -= 100
      let up = 5 + g.war / 15
      let p.train = min([100, p.train + up])
      call s:Log(printf('%s %sが訓練+%d (%d)', p.name, g.name, up, p.train))
      return ''
    elseif c ==# '6'      " 鉄砲購入 (文化度が高いと南蛮商人が良くしてくれる)
      if p.gold < 300 | call s:Pause('金が足りません(300必要)。') | continue | endif
      let p.gold -= 300
      let got = 20 + s:Culture(s:player) / 10
      let p.guns += got
      call s:Log(printf('%s 南蛮商人から鉄砲%d購入 (計%d)', p.name, got, p.guns))
      return ''
    elseif c ==# '7'
      if s:CmdAttack(a:pi) | return '' | endif
    elseif c ==# '8'
      if s:CmdTransport(a:pi) | return '' | endif
    elseif c ==# '9'
      if s:CmdDiplo(a:pi) | return '' | endif
    elseif c ==# 'T' || c ==# 't'
      if s:CmdTea(a:pi) | return '' | endif
    elseif c ==# 'C' || c ==# 'c'
      if s:CmdCourt(a:pi) | return '' | endif
    elseif c ==# 'N' || c ==# 'n'
      if s:CmdNanban(a:pi) | return '' | endif
    elseif c ==# 'B' || c ==# 'b'
      call s:ShowGenerals()
    elseif c ==# 'A'      " 委任
      let p.auto = 1
      call s:Log(printf('%s を委任 (以後は家臣が差配)', p.name))
      call s:AIProvAct(s:player, a:pi)
      return ''
    elseif c ==# 'R'
      call s:CmdRelease()
    elseif c ==# 'i' || c ==# 'I'
      call s:ShowInfo()
    elseif c ==# '0' || c ==# ' '
      return ''
    elseif c ==# 'E'
      return 'END'
    elseif c ==# 'S'
      call s:Save()
      call s:Pause('セーブしました: ' . s:SAVEFILE)
    elseif c ==# 'Q'
      redraw
      if input('本当に降参して終了しますか? (yes/no): ') ==# 'yes'
        throw 'SENGOKU_QUIT'
      endif
    endif
  endwhile
endfunc

func! s:PlayerTurn() abort
  if s:player < 0 | return | endif
  let skip = 0
  let prompted = 0
  " 直轄国を先に、委任国は残った武将で後から差配する
  let autos = []
  for pi in s:ClanProvs(s:player)
    if s:prov[pi].owner != s:player | continue | endif
    if s:prov[pi].auto
      call add(autos, pi)
      continue
    endif
    if skip | continue | endif
    let prompted = 1
    if s:PlayerCommand(pi) ==# 'END'
      let skip = 1
    endif
  endfor
  for pi in autos
    if s:prov[pi].owner != s:player | continue | endif
    call s:AIProvAct(s:player, pi)
  endfor
  " 全国委任中でも月ごとに操作機会を残す
  if !prompted && !skip
    while 1
      call s:Render(-1)
      redraw
      echo '全国委任中…  Enter:次の月へ R:委任解除 i情報 S保存 Q降参 > '
      let raw = getchar()
      let c = type(raw) == type(0) ? nr2char(raw) : ''
      if c ==# 'R'
        call s:CmdRelease()
      elseif c ==# 'i' || c ==# 'I'
        call s:ShowInfo()
      elseif c ==# 'S'
        call s:Save()
        call s:Pause('セーブしました: ' . s:SAVEFILE)
      elseif c ==# 'Q'
        redraw
        if input('本当に降参して終了しますか? (yes/no): ') ==# 'yes'
          throw 'SENGOKU_QUIT'
        endif
      else
        return
      endif
    endwhile
  endif
endfunc

" ---------------------------------------------------------------------
" 月末処理
" ---------------------------------------------------------------------
func! s:MonthEnd() abort
  for p in s:prov
    if p.owner < 0 | continue | endif
    " 商業収入 (文化度が高いほど増える)
    let p.gold += p.comm * (100 + s:Culture(p.owner)) / 100
    let p.rice -= p.sol / 50
    if p.rice < 0
      let desert = -p.rice * 2
      let p.sol = max([0, p.sol - desert])
      let p.rice = 0
      let p.loyal = max([0, p.loyal - 3])
      if p.owner == s:player
        call s:Log(printf('%s 兵糧が尽き%d人が逃散……', p.name, desert))
      endif
    endif
    if s:month % 3 == 0
      let p.train = max([20, p.train - 1])
    endif
    if s:month == 9
      let rate = 70 + s:Rand(61)
      let harvest = p.koku * 3 * rate / 100
      let p.rice += harvest
      if p.owner == s:player
        if rate >= 120
          call s:Log(printf('%s 豊作! 米+%d', p.name, harvest))
        elseif rate <= 80
          call s:Log(printf('%s 凶作…… 米+%d', p.name, harvest))
        endif
      endif
    endif
    if (s:month == 8 || s:month == 9) && s:Rand(100) < 12
      let dmg = p.koku / 20
      let p.koku -= dmg
      if p.owner == s:player
        call s:Log(printf('%s 台風襲来! 石高-%d', p.name, dmg))
      endif
    endif
    if p.loyal < 25 && s:Rand(100) < 30
      let loss = p.sol / 5
      let p.sol -= loss
      let p.koku = p.koku * 95 / 100
      let p.loyal += 10
      call s:Log(printf('%s 一揆勃発! 兵-%d 石高減', p.name, loss))
    endif
  endfor
  call s:MerchantEvents()
  call s:DiploEvents()
  let s:month += 1
  if s:month > 12
    let s:month = 1
    let s:year += 1
  endif
  " 武将は月が替わると再び動ける
  call s:ResetGens()
endfunc

func! s:UnownedItems() abort
  return filter(range(len(s:items)), 's:ItemOwner(v:val) < 0')
endfunc

func! s:MerchantEvents() abort
  " 堺の商人がプレイヤーに名物を売りに来る
  let pool = s:UnownedItems()
  if empty(pool) | return | endif
  if s:player >= 0 && s:ClanAlive(s:player) && s:Rand(100) < 10
    let ii = pool[s:Rand(len(pool))]
    let item = s:items[ii]
    let src = s:RichestProv(s:player)
    if src >= 0 && s:prov[src].gold >= item.price
      call s:Render(-1)
      redraw
      let q = printf('堺の商人が名物「%s」を持って参上 (値段 金%d / %sの金%d / 文化+%d)。買いますか? (y/n): ',
            \ item.name, item.price, s:prov[src].name, s:prov[src].gold, item.cul)
      if input(q) =~? '^y'
        let s:prov[src].gold -= item.price
        call s:GainItem(s:player, ii)
        call s:Log(printf('名物「%s」を購入! 文化+%d (%d)', item.name, item.cul, s:Culture(s:player)))
      else
        call s:Log(printf('名物「%s」の購入を見送った', item.name))
      endif
    endif
  endif
  " AI大名も茶器を買い集める (買える名物の中から選ぶ)
  let pool = s:UnownedItems()
  if !empty(pool) && s:Rand(100) < 8
    " 最安の名物を買える家に絞ってから抽選
    let minp = min(map(copy(pool), 's:items[v:val].price'))
    let cands = filter(range(len(s:clans)),
          \ 'v:val != s:player && s:ClanAlive(v:val)'
          \ . ' && s:RichestProv(v:val) >= 0'
          \ . ' && s:prov[s:RichestProv(v:val)].gold >= minp')
    if !empty(cands)
      let ci = cands[s:Rand(len(cands))]
      let src = s:RichestProv(ci)
      let afford = filter(copy(pool), 's:items[v:val].price <= s:prov[src].gold')
      let ii = afford[s:Rand(len(afford))]
      let s:prov[src].gold -= s:items[ii].price
      call s:GainItem(ci, ii)
      call s:Log(printf('%s家が名物「%s」を入手したとの噂', s:clans[ci].name, s:items[ii].name))
    endif
  endif
endfunc

func! s:DiploEvents() abort
  " AI同士の同盟締結
  if s:Rand(100) < 12
    let cands = filter(range(len(s:clans)), 'v:val != s:player && s:ClanAlive(v:val)')
    if len(cands) >= 2
      let a = cands[s:Rand(len(cands))]
      let bs = filter(copy(s:NeighborClans(a)), 'v:val != s:player && !s:IsAlly(a, v:val)')
      if !empty(bs)
        let b = bs[s:Rand(len(bs))]
        call s:SetAlly(a, b, 1)
        call s:Log(printf('%s家と%s家が同盟を結んだ', s:clans[a].name, s:clans[b].name))
      endif
    endif
  endif
  " AI同士の同盟解消
  let aikeys = filter(keys(s:ally),
        \ 'split(v:val, "-")[0] != s:player && split(v:val, "-")[1] != s:player')
  if !empty(aikeys) && s:Rand(100) < 3
    let key = aikeys[s:Rand(len(aikeys))]
    let pair = split(key, '-')
    call remove(s:ally, key)
    call s:Log(printf('%s家と%s家の同盟が手切れに',
          \ s:clans[str2nr(pair[0])].name, s:clans[str2nr(pair[1])].name))
  endif
  " AIからプレイヤーへの同盟の申し出
  if s:player >= 0 && s:ClanAlive(s:player) && s:Rand(100) < 4
    let cands = filter(s:NeighborClans(s:player), '!s:IsAlly(s:player, v:val)')
    if !empty(cands)
      let ci = cands[s:Rand(len(cands))]
      call s:Render(-1)
      redraw
      let q = printf('%s家(%s)より同盟の申し出が届いた。受けますか? (y/n): ',
            \ s:clans[ci].name, s:clans[ci].daimyo)
      if input(q) =~? '^y'
        call s:SetAlly(s:player, ci, 1)
        call s:Log(printf('%s家と同盟成立!', s:clans[ci].name))
      else
        call s:Log(printf('%s家の同盟の申し出を断った', s:clans[ci].name))
      endif
    endif
  endif
endfunc

func! s:CheckEnd() abort
  if s:player < 0 | return 0 | endif
  if !s:ClanAlive(s:player)
    call s:Pause(printf('%s家は滅亡しました…… 【ゲームオーバー】', s:clans[s:player].name))
    return 1
  endif
  if len(s:ClanProvs(s:player)) == len(s:prov)
    call s:Pause(printf('全国統一!! %s(%s家)は天下人となった! 【勝利】',
          \ s:clans[s:player].daimyo, s:clans[s:player].name))
    return 1
  endif
  return 0
endfunc

" ---------------------------------------------------------------------
" セーブ / ロード
" ---------------------------------------------------------------------
func! s:Save() abort
  let data = {'ver': s:SAVEVER, 'year': s:year, 'month': s:month,
        \ 'player': s:player, 'prov': s:prov, 'log': s:log[-30 :],
        \ 'cstate': s:cstate, 'ally': s:ally}
  call writefile([json_encode(data)], s:SAVEFILE)
endfunc

func! s:Load() abort
  if !filereadable(s:SAVEFILE) | return 0 | endif
  try
    let data = json_decode(join(readfile(s:SAVEFILE), ''))
  catch
    return 0
  endtry
  if type(data) != type({}) || get(data, 'ver', 0) != s:SAVEVER | return 0 | endif
  call s:InitState()
  let s:year = data.year
  let s:month = data.month
  let s:player = data.player
  let s:prov = data.prov
  let s:log = data.log
  let s:cstate = data.cstate
  let s:ally = data.ally
  return 1
endfunc

" ---------------------------------------------------------------------
" ゲーム進行
" ---------------------------------------------------------------------
func! s:GameLoop() abort
  call s:OpenBuf()
  try
    while 1
      call s:PlayerTurn()
      call s:AITurns()
      if s:CheckEnd() | break | endif
      call s:MonthEnd()
      if s:CheckEnd() | break | endif
    endwhile
  catch /SENGOKU_QUIT/
    redraw
    echo '乱世に幕を下ろした。また会おう。(:Sengoku で再挑戦 / :SengokuLoad で再開)'
  catch /^Vim:Interrupt$/
    call s:Save()
    redraw
    echo '中断しました。自動セーブ済み → :SengokuLoad で再開できます。'
  finally
    call s:CloseUI()
  endtry
endfunc

func! sengoku#Start() abort
  call s:InitState()
  let s:log = []
  call s:OpenBuf()
  call s:Render(-1)
  redraw
  let items = map(range(len(s:clans)),
        \ 'printf("%s家 %s (戦%d 政%d 魅%d) 本拠:%s", s:clans[v:val].name, s:clans[v:val].daimyo,'
        \ . ' s:clans[v:val].war, s:clans[v:val].pol, s:clans[v:val].chr,'
        \ . ' join(map(s:ClanProvs(v:val), "s:prov[v:val].name"), "・"))')
  let sel = s:Menu(printf('== 大名家を選んでください (全%d家) ==', len(s:clans)), items)
  if sel < 0
    call s:CloseUI()
    echo '中止しました。'
    return
  endif
  let s:player = sel
  call s:Log(printf('%s家でゲーム開始! 目指せ天下統一!', s:clans[sel].name))
  call s:GameLoop()
endfunc

func! sengoku#LoadStart() abort
  if !s:Load()
    echo 'セーブデータがありません(または旧版のデータです): ' . s:SAVEFILE
    return
  endif
  call s:GameLoop()
endfunc

" ---------------------------------------------------------------------
" セルフテスト
" ---------------------------------------------------------------------
func! sengoku#SiegeTest() abort
  " AI同士で籠城戦を複数回回してロジックを検証
  let results = {}
  for i in range(6)
    call s:InitState()
    let s:log = []
    let s:player = -1
    call s:OpenBuf()
    let from = s:PIdx('尾張')
    let to = s:PIdx('三河')
    let oda = s:CIdx('織田')
    let f = s:prov[from]
    let f.sol = 9000
    let f.guns = 60
    let f.train = 70
    let s:acan = i % 3
    let lines = []
    let r = s:SiegeBattle(oda, from, to, 4000 + i * 1000, 30 + i * 10, f.train,
          \ s:gens[oda][i % 8], lines)
    let key = s:bt.result
    let results[key] = get(results, key, 0) + 1
    for p in s:prov
      if p.sol < 0 || p.guns < 0
        echoerr 'SIEGETEST FAIL: 負の値 ' . p.name
        return
      endif
    endfor
    if r.win && s:prov[to].owner != oda
      echoerr 'SIEGETEST FAIL: 勝利したのに領有が変わっていない'
      return
    endif
    if !r.win && s:prov[to].owner == oda
      echoerr 'SIEGETEST FAIL: 敗北したのに領有が変わった'
      return
    endif
  endfor
  echom 'SIEGETEST OK: 結果内訳 ' . string(results)
  call s:SiegeRender({})
  call writefile(getline(1, '$'), get(g:, 'sengoku_siege_dump', '/dev/null'))
  call s:CloseUI()
endfunc
func! sengoku#SelfTest() abort
  call s:InitState()
  let s:log = []
  " マップ整合性
  let errs = s:ValidateMap()
  if !empty(errs)
    for e in errs
      echoerr 'MAP FAIL: ' . e
    endfor
    return
  endif
  echom printf('MAP OK: %d国 %d家 隣接対称・グリッド衝突なし', len(s:prov), len(s:clans))
  " 武将データ
  let ngens = 0
  for ci in range(len(s:clans))
    if len(s:gens[ci]) != 8
      echoerr printf('GEN FAIL: %s家の武将が%d人', s:clans[ci].name, len(s:gens[ci]))
      return
    endif
    for g in s:gens[ci]
      if g.war <= 0 || g.war > 100 || g.pol <= 0 || g.pol > 100
        echoerr 'GEN FAIL: 能力値が不正 ' . g.name
        return
      endif
    endfor
    let ngens += len(s:gens[ci])
  endfor
  echom printf('GEN OK: 全%d家 x 8人 = %d武将', len(s:clans), ngens)
  " 全AIで36ヶ月
  let s:player = -1
  for i in range(36)
    call s:AITurns()
    call s:MonthEnd()
  endfor
  let alive = []
  for ci in range(len(s:clans))
    if s:ClanAlive(ci)
      call add(alive, printf('%s:%d', s:clans[ci].abbr, len(s:ClanProvs(ci))))
    endif
  endfor
  let total = 0
  for p in s:prov
    let total += p.sol
    if p.sol < 0 || p.rice < 0 || p.gold < 0 || p.cannon < 0
      echoerr 'SELFTEST FAIL: 負の値 ' . string(p)
      return
    endif
  endfor
  " 茶器の所有が重複していないか
  let owned = 0
  for ii in range(len(s:items))
    let n = 0
    for ci in range(len(s:clans))
      let n += count(s:cstate[ci].items, ii)
    endfor
    if n > 1
      echoerr 'SELFTEST FAIL: 茶器の所有重複 ' . s:items[ii].name
      return
    endif
    let owned += n
  endfor
  " 同盟キーの正当性
  for key in keys(s:ally)
    let pair = split(key, '-')
    if len(pair) != 2 || str2nr(pair[0]) >= str2nr(pair[1])
      echoerr 'SELFTEST FAIL: 不正な同盟キー ' . key
      return
    endif
  endfor
  let diplo = len(filter(copy(s:log), 'v:val =~# "同盟"'))
  echom printf('SELFTEST OK: %d年%d月 生存%d家 [%s] 総兵力=%d 茶器所有%d/%d 同盟中%d件 外交事件%d件',
        \ s:year, s:month, len(alive), join(alive, ' '), total,
        \ owned, len(s:items), len(s:ally), diplo)
  call s:OpenBuf()
  call s:Render(0)
  echom 'RENDER OK: ' . line('$') . ' lines'
  if s:UseFloat()
    for key in ['power', 'log']
      let st = get(get(s:, 'popups', {}), key, {})
      if empty(st) || get(st, 'win', -1) == -1 || !nvim_win_is_valid(st.win)
        echoerr 'POPUP FAIL: ' . key . ' ポップアップが表示されていない'
        return
      endif
      if empty(join(nvim_buf_get_lines(st.buf, 0, -1, v:false), ''))
        echoerr 'POPUP FAIL: ' . key . ' の中身が空'
        return
      endif
    endfor
    echom 'POPUP OK: 勢力+戦況ポップアップをマップ上に表示'
  endif
  call s:CloseUI()
endfunc
