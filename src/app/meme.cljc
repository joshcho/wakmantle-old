(ns app.meme
  #?(:clj (:import
           [java.util Collections ArrayList Random Collection]
           ;; (clojure.core.cache LRUCache)
           ))
  (:require
   contrib.str
   ;; #?(:clj [datascript.core :as d])
   [hyperfiddle.electric :as e]
   [hyperfiddle.electric-dom2 :as dom :refer [div input props on text style a button span]]
   [hyperfiddle.electric-ui4 :as ui]
   [hyperfiddle.electric-svg :as svg :refer [svg circle g rect polygon line]]
   ;; [goog.string :as gstring]
   [missionary.core]
   [app.utils :as u :refer [cond-let cmt enter arrows mb-> mb->> mb->>> dropdown
                            >defn >fn letm]]
   [app.central :as central :refer [defstate]]
   [app.tailwind :refer [tw -tw]]
   #?(:clj
      [clojure.tools.macro :refer [macrolet symbol-macrolet]])
   #?(:clj
      [app.data :refer [우왁굳 아이네 징버거 비챤 릴파 주르르 고세구 gomem1 gomem2] :as data])
   [app.printer :as p]
   [app.kr :as kr]
   [clojure.string :as str]
   [clojure.set :as set]
   [clojure.core.matrix :as mat]
   [malli.core :as m]
   ;; [clojure.core.cache :as cache]
   #?(:clj
      [clojure.core.cache.wrapped :as w]
      )
   ;; #?(:clj
   ;;    [clojure.core.cache :as cache])
   #?@
   (:cljs
    [
     ["remarkable" :as remarkable]
     ["remarkable-katex" :as remarkable-katex]
     [goog.dom :as gdom]
     ;; [goog.dom.element :as gdom.element]
     ]
    :clj
    [[wkok.openai-clojure.api :as api]
     ;; [reduce-fsm :as fsm]
     [clojure.core.async :as a]
     ])))

#?(:clj
   (do

     (def extra-meme-description-map
       (->> (str/split data/raw-composed-pairs-text
                       #"\n\n")
         (map (fn [pair-text]
                (mapv str/trim (str/split pair-text #"\n" 2))))
         (into {})))
     (defn get-embeddings [strings]
       (assert (coll? strings))
       (->> (api/create-embedding
             {:model "text-embedding-ada-002"
              :input strings})
         :data
         (sort-by :index)
         (map :embedding)))
     (defonce get-embeddings-memo (memoize get-embeddings))))

#?(:clj
   (do
     (def owner-meme-info-map
       (letfn [(handle-colon [streamer-var]
                 (->> (str/split streamer-var (re-pattern "\n+"))
                   (map #(str/split % (re-pattern ": ") 2))
                   (filter
                    (fn [x]
                      (= (count x) 2)))
                   (map (fn [[x y]]
                          [(str/trim x) y]))))
               (handle-gomem [x]
                 (let [[s1 s2] x]
                   (->> (str/split (str s1 "\n" s2)
                                   (re-pattern "\n+"))
                     (map #(str/split % (re-pattern ": ") 2))
                     (filter
                      (fn [x]
                        (= (count x) 2)))
                     (map (fn [[x y]]
                            [(str/trim x) y])))))]
         (->> (merge
               {nil
                (merge
                 (dissoc extra-meme-description-map
                         "아이네" "징버거" "릴파" "주르르" "고세구" "비챤")
                 {"맏언니즈"                        "팀 내 최연장자 라인. 언니즈라는 명칭이 있지만, 이는 릴파까지 포함한 3인 조합에 더 자주 쓰이기 때문에 보통은 맏언니즈 내지 큰언니즈로 불린다. 아이네는 이세돌의 둘째인 징버거를 존칭 없이 '우리 버거'나 '버거야'라고 부를 수 있는 유일한 멤버이기에, 조심성 많은 성격인 징버거도 맏언니 아이네에게 차츰차츰 마음을 열며 의지하는 모습을 보여 아이네가 감동을 받았다고 한다. 쑥스러움을 많이 타는 징버거를 아이네가 놀리거나 챙겨주는 모습이 많이 관찰되지만, 징버거를 필두로 동생들이 합심해 맏언니 아이네에게 장난을 걸고 반응을 즐기는 모습도 자주 보인다."
                  "메보즈"                          "아이네와 릴파가 이세돌의 메인보컬로 분류되는 것에 영향을 받아 생겨난 조합. 두 사람이 사용하는 아바타의 제작자가 같기 때문에 자매라고 표현하기도 한다. 아이네 본인의 말에 따르면 애증의 관계인데, 원래 아이네는 조용하고 차분한 성향이라 이세돌 결성 초기에 합방을 하면 혼자 구석에 가서 사람들을 구경하고 있는 경우가 많았다. 그런데 릴파는 이런 모습을 보면 오히려 더 말을 붙이며 친해지고 싶어 하는 타입이라 적극적으로 아이네를 따라다니며 장난을 걸기 시작했고 결국 아이네의 숨겨진 분노를 일깨워 버린 바람에 서로 투닥투닥거리다 가장 먼저 친해져 버렸다. 처음에는 릴파가 장난을 치기 시작하면 아이네는 그 모습을 지켜보며 마음속으로만 꾹꾹 눌러담거나 이따금 한 번씩 크게 터뜨리는 패턴이었지만, 친분이 두터워지면서 아이네가 악질네로 변신해 먼저 릴파를 놀리는 경우도 많이 늘었다. 또한 지금은 금지된 밈인 뢴르르처럼 같은 타겟을 향해 함께 악질짓을 하는 경우도 간혹 있다. It Takes Two를 같이 플레이했는데, 아이네가 릴파랑 어울리며 많이 밝고 활달해졌음에도 워낙 릴파가 에너지가 넘치다 보니 갈수록 기가 빨리는 모습을 보여주었다."
                  "은발즈"                          "아이네 고세구 두 멤버의 머리 색깔이 하얀색 계열인 점에서 유래된 조합으로, 백발즈나 네고즈라고도 불린다. 초반에는 주로 고세구가 아이네에게 먼저 장난을 거는 전개가 많았는데, 장난스러운 카톡을 보낸다거나 얼토당토 않은 말을 늘어놓으며 친해지고 싶은 마음에 좋아하는 사람들에게 장난을 자주 걸었다고 한다. 그 때문인지 아이네-릴파가 서로의 약점을 잡아서 놀리거나 투닥거리는 연년생 자매, 혹은 실친 같은 느낌이라면 아이네-고세구는 나이 차이가 좀 있는 자매 같다는 반응이 많다. 고세구는 아이네에 대해 웃음이 많고 장난을 잘 받아주는 상냥한 언니라는 말과 함께 자주 애정을 표현하고 있으며, 아이네도 고세구에 대해 굉장히 투명하고 솔직하며, 당차고 씩씩해서 걱정을 안 하게 만드는 동생이라는 말과 함께 애정을 표현한다. 고세구의 커버 곡 팬서비스 영상에서는 아이네가 조연으로 등장했고 이후 고세구의 부탁으로 나의 천사를 불러주면서 세계관이 연결되는 케미를 보였다."
                  "맏막즈"                          "아이네 비챤 둘 다 순둥순둥하고 남을 잘 배려하는 성격인지라 투닥투닥하는 케미보다는 서로 잔잔하게 귀여운 장면을 자주 보여준다. 징버거와 고세구가 자신을 사이에 두고 서로 본인에게 의지하라고 이야기하는(하지만 그 와중에도 서로를 고로시하는 데 여념이 없던) 클립을 본 비챤이 가장 먼저 찾은 멤버가 바로 아이네였을 만큼 평소에도 많이 의지하는 편인 듯하며, 아이네도 막내 비챤을 살갑게 잘 챙겨주는 모습이 많이 보인다. 이세돌 단체 대화방에서 엄하거나 약간 수위 높은 대화가 오갈 때면 아이네는 \"챤이 있잖아! 그런 말 하면 어떡해!\"라고 이야기한다는데, 비챤의 말에 따르면 그럴 땐 아이네가 자기를 거의 세 살짜리 아기로 대하는 기분이 든다고 한다."
                  "퐉스즈"                          "왁타버스 영상에서 유래되었다. 주르르와 징버거 둘 다 사람들을 잘 홀리는 여우 같은 상인데다, 고세구나 비챤 등을 잘 놀리며 장난치는 주르르가 징버거한테는 상냥하게 잘 대해주는 모습이 발견된 뒤 나타난 조합. 물론 딜 넣을 때는 잘 넣는다. 주르르의 발언에 의하면 자신은 의도적으로 시청자들을 유혹하는, 이른바 ‘만들어진 여자친구’ 느낌이라면 징버거는 자연스러운 행동 속에서 시청자들을 홀리는 ‘현실 여자친구’ 느낌이라고. 재미있는 건, 징버거는 '여우라는 말 안 좋은 뜻 아냐?'라며 시청자들에게 진지하게 물어본 적이 있다. 또한 둘 다 팬덤명의 유래가 같은 개과인 개(똥강아지)와 여우(주폭도)다. 서로가 더 퐉스라고 주장하는 관계이며, 이세돌 서열 정리에서는 '이세돌들 중 내가 제일 섹시하다'라는 질문에 둘 다 2위를 선택해 겹치는 장면이 나오기도 했다."
                  "107즈"                           "방송에서 한 IQ 테스트의 결과가 고세구와 동일한 107이 나와서 둘을 107즈라고 묶는다. 이 둘은 오리지널 곡 이세돌 싸이퍼의 3절 가사를 같이 불렀고, 2022년 5월 20일 징버거의 유튜브에서 공개된 TOMBOY 커버 영상에서는 메인보컬과 피처링을 담당해 각자의 스타일을 보여주었다. 이때 평소의 방송 아바타와는 많이 다른 느낌인 기네스버거와 눈나구 아바타로 각각 등장해서인지 눈나즈라는 새로운 이름으로 불리기도 한다. 둘 다 솔직하고 엉뚱한 소리를 많이 하는 편이라 상대방이 우스꽝스러운 소리를 했거나 상대방을 놀린 클립을 보고 반응을 주고받는 경우가 많다. 징버거가 고세구의 콘텐츠에 깜짝 등장하기도 하고, 징버거의 새로운 아바타 공개나 커버 곡에 고세구의 유니티 프로그램이 사용되기도 하는 등 방송적으로도 서로 협력하며 좋은 케미스트리를 보여주고 있다. 2023년 6월 27일 고세구의 트위치의 채널에서 케미 프로필 듀오 합방 컨텐츠를 진행하였다."
                  "개나리즈"                        "과거 ‘챤버거’로도 묶였던 사이. 이세돌 결성 초창기에 워낙 죽이 잘 맞던 두 멤버를 보며 만들어진 조합이었으나, 팬아트 등에서 이세돌 멤버들을 미묘하게 엮는(?) 경우가 자주 발생하자 논란을 의식한 우왁굳이 생방송과 왁물원 공지를 통해 백합으로 오해받을 수 있는 2차 창작 활동을 금지시키면서 도매금으로 챤버거도 금지당했다. 유사 백합물 현 시점에서는 두 사람의 퍼스널 컬러가 노란색과 초록색 계열인 것에 착안, 색 배합이 비슷한 개나리꽃의 이름을 딴 개나리즈로 불린다. 징버거와 비챤의 팬덤인 똥강아지와 라니단의 어감이 떠오르는 건 덤. 징버거가 비챤에게 붙인 애칭은 아기새. 23년 8월 17일에 진행한 이세돌 여름캠프 합방에서도 개나리즈로 묶였다. ???:개랑 라니해서 개라니즈아니에요? 둘 다 공포류라면 질색하지만 여름캠프 담력시험에서는 언니인 징버거가 동생인 비챤을 지키고자 하는 마음에 각성했는지 벌벌떠는 비챤을 격려해가며 게임을 클리어했다."
                  "부산즈"                          "부산 출신인 멤버 두 명을 묶어 새롭게 탄생한 조합. 특유의 '퐉스미'를 베이스로 다른 사람들과 쉴 틈 없이 티키타카를 주고받으며 분위기를 이끄는 주르르와 '릴트리버'라는 별명이 붙을 정도로 발랄하고 활기찬 릴파의 케미가 백미이며, 이세돌 싸이퍼의 2절을 함께 부르기도 한 사이이다. 사실 이세돌 멤버들에게 장난치는 걸 워낙 좋아하던 주르르가 유독 릴파에게만은 장난을 못 걸고 얌전해지는 모습이 종종 보였는데, 서로를 릴파땅과 르르땅rr땅이라고 부르며 드립과 장난을 주고받는 지금 시점에서도 여전히 릴파 쪽이 더 적극적이라서 '주르르의 수비 포지션을 볼 수 있는 흔치 않은 기회'라며 주폭도들이 꽤나 좋아하는 조합 중 하나다. 2022년 2월 14일 릴파의 유튜브 채널에서 공개된 세계는 사랑에 빠져있어 커버 영상에 각각 주연과 조연으로 출연해 처음 언급되었고, 이후 2022년 3월 14일 주르르의 유튜브 채널에 올라온 사이언티스트 영상에 세사빠 때와 똑같은 의상을 입은 모습이 나오자 주르르가 직접 ‘세계관이 연결된 상태’라고 설명해 사실상 공식적인 케미가 되었다. 한편, 이세계아이돌의 '중간 나잇대'라는 특징을 갖고 있는 멤버들이기도 하다 보니 우왁굳은 이 둘의 조합에 어떤 이름을 붙여 줘야 할지 고민하다 중간즈라고 부르기도 했다."
                  "고릴듀오"                        "우왁굳의 마인크래프트 오징어 게임 콘텐츠 때 불법 합방으로 함께 우왁굳의 방송을 보게 되면서 탄생한 조합이며, 각각 앞글자를 따서 고릴듀오로 불린다. 방송에서 어떠한 주제가 생기면 이를 이세돌과 연결시켜 스토리를 만들어낸다는 공통점이 있으며, 2022년 2월 14일 릴파 채널에서 공개된 '세계는 사랑에 빠져있어' 커버 영상의 스토리에 대한 릴파와 고세구의 해석 과정이 유사해 다시 한 번 떠오르기도 했다. 또한 둘 다 텐션이 매우 높아 같이 엮이게 되면 굉장히 떠들썩해지는데, ASMR과 춤 맛보기 합방에서 높은 텐션을 보여줬으며, VRChat 등을 통해 합방을 하게 되면 즉석에서 함께 춤을 추기도 하는 등 좋은 케미스트리를 보여주고 있다. 2022년 9월 24일 마인크래프트 팬덤전쟁 콘텐츠에서 임시동맹을 결성하여 활약했다. 23년 8월 17일 이세돌 여름캠프 합방에서도 고릴듀오로 묶였다. 보통은 릴파가 이끌고 고세구가 맞장구쳐주는 그림이 강하지만 공포류에서는 예외인데 고세구는 공포를 좋아하는 반면 릴파는 겁쟁이 트리오로 묶이므로 고세구 뒤에 숨는 릴파와 그런 릴파를 놀려먹는 고세구가 일품이다."
                  "베란다즈"                        "릴파가 어렸을 때 베란다에서 병아리를 키웠던 것처럼 비챤도 집으로 데려와서 키우고 싶다고(애완동물처럼 베란다에 가두어 두고 키우고 싶을 만큼 비챤이 너무 귀엽다는 말.) 덧붙이며 싫다고 투정을 부리는 듯했지만, 어느새 방송의 배경 화면을 베란다 이미지로 바꾸며 \"제가 언니한테 말해서 50평으로 확장공사 시켜달라고 했거든요!\" 라고 말하는 비챤과 그 말을 듣자마자 텐션이 한 없이 높아져 즐거워하는 릴파의 모습이 돋보인 케미다. 릴파는 평소에도 비챤을 매우 예뻐하고, 비챤은 릴파의 아바타가 가장 본인 취향이라고 말하는 등 서로를 아끼는 모습을 보여주기 때문에 이 둘의 티키타카가 극대화되면 귀여움에 귀여움이 더해졌다며 박쥐단과 라니단의 채팅 화력이 어마어마해지는 건 덤. 비챤의 커버 곡 Butter에 릴파가 피처링으로 참여하기도 했다. 둘다 각각 v급밴드와 박취더락으로 밴드를 이끌고 있기도 하다"
                  "무7즈"                           "주르르의 클립을 본 고세구가 자기도 모르게 \"무7련\"이라고 시원하게 외친 사건에서 따온 조합이라 '무7듀오', 서로 흡연자라고 장난을 친 클립이 유명해 '담탐듀오'라고도 불린다. 죽이 상당히 잘 맞는 데다 아무리 그래도 아이돌을 표방하는 그룹 치고 걸기 쉽지 않은 디스인데, 둘 다 비슷한 결의 장난을 좋아하는 스타일이라 티키타카가 잘 된다. 주르르 본인 말에 따르면 \"이런 장난을 다 받아줄 만큼 세구가 착하다. 세구와 장난칠 때는 눈싸움 하면서 안에 돌 집어넣고 던지는 느낌\"이라고. 고세구 또한 친구 같은 언니이기에 언니라는 호칭보다 르르땅이나 방송명인 주르르라는 이름으로 부르는 빈도가 잦다. 이세돌 커버 곡 등 단체 녹음 시, 데뷔 전에 따로 녹음 경험이 없었던 주르르와 고세구만 항상 재녹음을 하고 있다는 주르르의 발언을 통해 재녹즈, 빠꾸즈라는 이름이 새롭게 생기기도 했다. 또한 이세돌 내에서 운전면허 보유자끼리 묶은 콤비이기도 하다. 지하아이돌과 주폭소년단 각각을 이끄는 사장이기도한 공통점이 있다"
                  "만만즈"                          "주로 핫걸 키워드로 연결되는 케미이며, VRChat 내부에서 각자의 복장 및 행동을 서로 단속하는 식으로 딜 교환을 나누거나 처음에는 주르르가 기강 잡는 듯 싶다가도 친한 자매처럼 서로 웃으면서 장난을 치는 관계이다. 언니가 만만해? 클립 마인크래프트 콘텐츠 '이세돌 팬덤전쟁'에서는 두 팀이 1차전부터 맞대결을 펼쳐 명장면 중 하나를 만들기도 했다. 2023년 6월 23일 둘이서 트위터 스페이스에서 방송을 하면서 만만즈라는 케미명이 생겼다."
                  "지구즈"                          "이세돌 내에서 가장 어린 두 명이 따로 묶이는 조합이며, 서로가 서로를 귀여워하고 힘이 되어주는 특이한 관계를 형성하고 있다. 비챤이 대등하게 장난을 걸 수 있는 거의 유일한 인물이기에 재미있는 클립들이 꽤 자주 올라오는 편. 고세구가 2022년 4월 27일 왁물원에 작성한 글에서 '자신과 비챤을 합쳐서 지구즈라고 부른다'는 언급을 해 지구즈라는 이름으로 통용되고 있으며, 막내즈, 잼민즈라고도 불린다. 고세구의 단독 콘서트 TOUR에서는 비챤이 깜짝 듀엣으로 나와 둘이 함께 노래를 부르기도 했다."
                  "언니즈"                          "각각 94년생 아이네, 95년생 징버거, 96년생 릴파로 그룹의 언니 라인을 담당하는 세 명이다. 이 영상에서 우왁굳의 지시를 받아 동생들을 집합시키거나 기강을 잡아야겠다는 식으로 장난스레 언급하는 일이 종종 있다. 보통 언니즈라고 한다면 이 3명을 뜻하지만 가장 언니인 아이네와 징버거를 언니즈 또는 맏언니즈, 릴파는 주르르와 함께 중간즈라고 부르기도 한다."
                  "전못진즈"                        "전하지 못한 진심 커버 곡 공개를 통해 생성된 조합으로, 이세계아이돌 최대 전력의 보컬 라인업이라고 불린다. 반쯤 농담으로는 헨타이즈를 견제하기 위한 조합이라고도 하고, 세 멤버 모두 동일한 제작자의 아바타를 사용하다 보니 간혹 자매라고 불리는 경우가 있다. 자주 투닥거리는 아이네와 릴파를 비챤이 중간에서 말리는 모습을 주로 볼 수 있었으며, 이세돌 멤버들 중에서도 덕력이 높은 멤버 세 명의 모임이라는 것에서 딴 씹덕즈라고 불리는 경우가 종종 있다. 왁타버스 VR챗 학교 합방 중 셋이서 떡볶이를 먹자는 얘기를 주고받았는데, 이를 엿듣던 징버거가 본인 빼고 먹는다는 사실을 깨닫고는 구석으로 들어가자 같이 데려가려고 했다며 달래주었다."
                  "나라카즈"                        "나라카 블레이드포인트 스쿼드 대회에 참가한 3인 케미이다. 대회 소식이 알려졌을 때 새로운 조합이라는 의견을 보인 팬들이 많았는데, 이윽고 \"이 조합은 우왁굳의 마인크래프트 오징어 게임 콘텐츠 때 불법(?) 합방으로 관전을 했던 닥트리오 조합이다\"라는 사실이 재조명되어 종종 닥트리오즈라고도 불린다. 이세돌내에서 모션 캡처 슈트를 보유한 3명이라 모캡즈라고 부를 수도 있다."
                  "헨타이즈"                        "이전부터 변태 상현 3인방으로 지목되어온 3명이 등장한 CH4NGE MMD 팬영상을 보던 멤버들의 채팅창에 처음 등장한 이후로 쭉 이렇게 불리고 있다. 징버거 본인은 이것보단 상술되어 있는 107즈, 무7즈의 7에서 따온 세븐즈라고 불렀으면 좋겠다고 언급했지만 이제는 본인도 받아들이고 꽤 잘 써먹는 중이라고 한다. 영문으로는 이니셜을 따온 HTZ로도 불린다. 아이네를 무척이나 영입하고 싶어하지만 아이네는 그 얘기를 들을 때마다 손사래를 친다. VR챗 축구 대전에서는 비챤을 합류시켜 레알 헨타이즈 CF 팀을 결성했으며, 팀은 16강에서 별 부수는 마담 팀에게 0-2로 패배했다."
                  "신호등즈"                        "각 멤버의 상징색을 신호등을 구성하는 3색(노랑, 빨강, 초록)에 빗대어 탄생한 케미이다. 우왁굳의 왁타버스 윷놀이 합방에서 징버거와 주르르가 같은 팀이 되어 비챤을 놀리는 그림이 나왔으며, 2022년 크리스마스에는 미리 메리 크리스마스 커버 곡을 함께 부르기도 했다."
                  "3만원즈"                         "제 1회 ER 인비테이셔널 대회에 나가 상금 3만원을 따온 에피소드에서 유래되었다. 이후 제 2회 ER 인비테이셔널 대회에서는 상금 24만원을 따오면서 24만원즈라고 불리기도 했다. 또한 이세돌 멤버들 중 3만원즈 세 명의 아바타가 먼저 바뀌면서 다시 재조명되기도 했다."
                  "유출즈"                          "방송에서 커버 곡 등의 작업물에 대해 의도치 않게 유출하거나 숨기려고 하는 게 티가 나는 등의 공통점이 있어 무7즈를 오마주한 유출즈(유7즈)라는 케미명이 붙게 되었다."
                  "동생즈"                          "각각 97년생 주르르, 98년생 고세구, 00년생 비챤으로 그룹의 동생 라인을 담당하는 세 명이다. 주르르가 나이에 비해 어른스러운 인상을 주는 반면, 고세구는 나이에 비해 훨씬 어린 인상을 주기 때문에 고세구와 비챤 두 명만 묶어 막내즈로 부르기도 한다. 케미 멤버 사이에 각각 한 살 터울, 두 살 터울이 있지만, 언니-동생 호칭보다는 르르땅, 택구, 챠니 등 애칭으로 부르며 친구처럼 지내는 것에 가깝다."
                  "히키퀸"                          "여자버전의 히키킹. 감다뒤 행동이라고 불리우지만 계속 등장한다."
                  "김릴파"                          "실제로 성을 공개하지는 않았지만 성을 붙여서 말하고 싶을 때 김씨를 붙여 김릴파라고 부른다."
                  "니인살"                          "니 인생 살아의 줄임말. 방종 화면에서 볼 수 있다."
                  "히키퀸스타즈"                    "각종 왁타버스 축구대회에 등장하는 팀. 히키킹의 밈 히키퀸을 따라 만들어진 이름."
                  "티바로살래나"                    "뢴트게늄이 감독으로 있는 버축대 팀."
                  "천양 다이노스"                   "각종 왁타버스 축구대회에 등장하는 팀. 천양이 감독."
                  "카오닝스타즈"                    "주닝요와 피카온이 감독으로 있는 버축대 팀."
                  "두세븐일레븐"                    "두칠이 감독으로 있는 버축대 팀."
                  "와크무런"                        "이니셜W에 등장을 처음하였다. 실제 그란대회 이름."
                  "창천스타즈"                      "와크무런 대회에서 시작된 팀. 남궁혁의 무협 컨셉에서 이름을 가져왔다."
                  "겜스터"                          "우왁굳의 게임 크루."
                  "우왁굳TV"                        "우왁굳의 이전 방송 크루."
                  "왁스비 보세요"                   "우왁굳 다시보기가 왁스비라고 불리던 시절, 무엇인가를 찾는 팬치에게 자주 하던 이야기"
                  "위윌왁휴"                        "스바라시 (스테이) 작곡가."
                  "버복대"                          "이세돌 대 버튜버로 진행한 버츄얼 복싱대회."
                  "버축대"                          "버튜버로 진행한 잔디 대회."
                  "IKU0019"                         "하쿠의 짭멤."
                  "왁빡오"                          "머리에 국자가 달린 왁파고의 짭멤."
                  "이덕구 할아버지"                 "이덕수 할아바이의 짭멤."
                  "헬로석"                          "해루석의 짭멤."
                  "비스니즈킴"                      "비즈니스킴의 짭멤."
                  "문신"                            "풍신의 짭멤."
                  "잡리더"                          "프리터의 짭멤."
                  "쿠키킹"                          "히키킹의 짭멤. 우왁굳이 히키킹 킹받게 할려고 뽑았다."
                  "단답벌"                          "단답벌레의 짭멤."
                  "노스트라담 후드"                 "노스트라투 호드의 짭멤"
                  "진짜 부정형인간"                 "부정형인간의 짭멤."
                  "동팡밍방상"                      "동글동글 빔을 맞은 도파민 박사의 짭멤."
                  "공개소녀"                        "소름돋는 싱크로율을 가진 비밀소녀의 짭멤."
                  "퀑민"                            "권민의 짭멤."
                  "엔젤"                            "우왁굳의 와이프, 김수현 아나운서를 지칭하는 말. 결혼 전부터 여자친구로써 엔젤이라고 불려져왔다."
                  "엔하"                            "엔젤 하이라는 뜻. 방송 중 엔젤님이 등장하면 채팅창에 등장한다."
                  "왁하"                            "우왁굳 하이라는 뜻."
                  "왁바"                            "우왁굳 바이라는 뜻."
                  "릴하"                            "릴파 하이라는 뜻."
                  "릴바"                            "릴파 바이라는 뜻."
                  "릴하다"                          "릴해의 변형. 초창기 릴파의 방송에서 노출 많은 아바타들을 보고 채팅에서 야해라고 쓰는 대신 릴해(릴파해)라고 부르던 밈이 굳어져서, 이세계아이돌 전체에서 쓰이는 밈이 되었다. 심지어 이세돌 멤버가 야하다고 말했는데 유튜브 편집자가 자막으로 릴하다고 바꿔서 쓰는 경우도 있다. 한편 릴파는 이런 반응에 대해 패션이라는 입장을 당당하게 드러내기도 했다. 단, 현재는 대부분의 의상을 자체 제작하며 디자인 자체를 의상 제작자의 몫으로 맡기는 일이 많아 이전의 과거 의상에 비해 배꼽 노출 면적이 줄어들어 릴하다는 반응이 적어진 편이었지만 키딩 단체 안무 영상에서 오랜만에 단체로 배꼽을 드러냈다."
                  "주하"                            "주르르 하이라는 뜻."
                  "징하"                            "징버거 하이라는 뜻."
                  "고하"                            "고세구 하이라는 뜻."
                  "챤하"                            "비챤 하이라는 뜻."
                  "주바"                            "주르르 바이라는 뜻."
                  "징바"                            "징버거 바이라는 뜻."
                  "고바"                            "고세구 바이라는 뜻."
                  "챤바"                            "비챤 바이라는 뜻."
                  "팬치"                            "우왁굳의 팬덤. 침팬치 그자체라 그래서 지어졌다."
                  "둘기"                            "아이네의 팬덤."
                  "똥강아지"                        "징버거의 팬덤."
                  "박쥐"                            "릴파의 팬덤."
                  "주폭도"                          "주르르의 팬덤."
                  "세균단"                          "고세구의 팬덤."
                  "고라니"                          "비챤의 팬덤."
                  "이파리"                          "이세계아이돌의 팬덤."
                  "왁물원"                          "우왁굳의 팬덤인 팬치들과 이세계아이돌의 팬덤인 이파리들이 있는 클린한 네이버 카페."
                  "이세페"                          "2023년 9월 23일 인천광역시 송도 달빛축제공원에서 개최된 국내 최초의 오프라인 메타버스 페스티벌이다. 이세계 페스티벌의 줄임말."
                  "이세계 페스티벌"                 "2023년 9월 23일 인천광역시 송도 달빛축제공원에서 개최된 국내 최초의 오프라인 메타버스 페스티벌이다."
                  "주스단"                          "주르르의 팬덤인 주폭도를 사람들이 잘못부른 것에서 나온 밈."
                  "아름다운 송도"                   "우왁굳의 송도 노가리를 왁컬로이드로 변화시킨 노래."
                  "돈까스"                          "우왁굳이 돈까스를 먹고 싶어하는 노래. 근본 왁컬로이드 노래이다."
                  "레고랜드"                        "왁물원에서 밴된 이들이 서식하는 카페."
                  "주폭소년단"                      "주르르의 0610 Production이 기획한 남자 아이돌 그룹."
                  "오이쿤"                          "주폭소년단의 연하남 그린."
                  "앙수"                            "주폭소년단의 버터음색 옐로우."
                  "채루미"                          "주폭소년단의 정열의 아이돌 레드."
                  "흰젓가락"                        "주폭소년단의 사연많은 블랙."
                  "윤대대"                          "주폭소년단의 쾌활 확생회장 바이올렛."
                  "설권"                            "주폭소년단의 병약소년 하늘색."
                  "지하아이돌"                      "고세구가 기획한 심상치 않은 여자 아이돌 그룹."
                  "띠뜨띠뜨공쥬짱짱걸"              "지하아이돌의 멤버. 띠뜨나라의 공주. 머리위의 소주 두 병과 상시 빨개진 얼굴이 특징이며 목을 쥐어짜는듯한 괴상한 말투는 그냥 술 주정이 아니라 띠뜨나라에서 쓰는 띠뜨체라고한다."
                  "띠뜨띠뜨"                        "지하아이돌의 멤버. 띠뜨띠뜨공쥬짱짱걸의 줄임말. 띠뜨나라의 공주. 머리위의 소주 두 병과 상시 빨개진 얼굴이 특징이며 목을 쥐어짜는듯한 괴상한 말투는 그냥 술 주정이 아니라 띠뜨나라에서 쓰는 띠뜨체라고한다."
                  "배춘희"                          "지하아이돌의 멤버. 전직 아이돌 출신이다. 말이 기본적으로 굉장히 많으며 옛날 밈부터 시작해서 최신 밈까지 온갖 인터넷 밈을 사용하며 양지와 음지를 넘나드는 모습을 보여준다. 멤버를 가리지 않고 딜 하고 탱도 서고 힐도 잘 해주는 이런 사람이 도대체 갑자기 어디서 튀어나왔나 싶을 정도로 말발에 재능이 있다."
                  "코드짱"                          "지하아이돌의 멤버. 코딩을 좋아하는 극도로 내성적인 찐따,너드 컨셉의 아이돌. 지하돌의 탱커로서 딜러들의 멘트를 원활하게 잘 받아주는 점을 높게 평가해 외핵으로 배정되었다. 그렇다고 마냥 맞는 건 아니고 쭈구린 채로 세게 한 방 날리기도 한다."
                  "코코미"                          "지하아이돌의 멤버. 도내 최상급 큐티빠띠뷰티 18살 현직 여고생. 음이탈 창법을 구사하며 띠뜨띠뜨와 함께 예능적인 보컬로서의 압도적인 존재감을 과시하고 있다. 고세구 피셜 지하아이돌 최고 싸이코이며 평소에는 꽤 얌전하다가 갑자기 소리를 지르거나 웃거나 울거나 하는 등 지하돌의 오버 리액션을 도맡아서 하고있다."
                  "써니"                            "지하아이돌의 멤버. 현실 걸그룹에서도 한 명 쯤은 있을법한 외국인 멤버이다. 아버지가 일본인이고 어머니가 애틀란타 사람이라 3개국어를 구사할 수 있다고 한다. 금발과 다른 멤버들의 1.5배에 달하는 매우 큰 키가 특징이며 외국인 그 자체의 부자연스러운 억양을 구사하면서도 와중에 들리는 한국인 저리 가라 할 정도의 훌룽한 딕션이 포인트이다."
                  "연토리 뿡치"                     "지하아이돌의 멤버. 이중인격, 얀데레 컨셉의 자칭 메인 래퍼. 연토리 뿡치라는 이름은 부모님이 지어주신 이름인데 이 이상한 이름 때문에 어렸을 때부터 놀림을 당해 히키코모리가 되어버렸다고 한다."
                  "우앵두"                          "지하아이돌의 멤버이자 리더. 비정상인들 사이에 유일한 정상인 포지션이며 오디션 때도 이런 포지션이 한 명 정도는 있으면 좋겠다는 고세구의 의견에 뽑히게 되었다. 리더 역할도 이 포지션 때문에 반쯤 짬 때려진 느낌이긴 하지만 합방 때마다 나름 멤버들을 이끌려고 노력한다. 프로듀스 지하돌때는 혼자 따로 앉아있던 뿡치 옆에 앉으며 챙겨주는 리더의 모습을 보여주기도 했다. 합방 때는 광대들로 가득 찬 혼란한 오디오에서 나름의 쉼터 역할을 해주고 있다."
                  "이노리"                          "지하아이돌의 멤버. 자기 자신을 이 몸이라고 부르는 자칭 마왕. 초면인 사람에게도 뒤에 인간이라는 호칭을 붙이며 반말을 힌다. 컨셉을 흔들려고 어떻게 질문을 해도 뻔뻔하게 받아치는 고정멤버급의 컨셉 유지력을 보여주고 있다."
                  "켄노"                            "지하아이돌의 멤버. 공기 반 소리 반을 넘어서는 공기 100 소리 0에 가까운 특이한 발성법을 가진 아이돌. 착해 보이는 얼굴과 목소리 톤과는 다르게 지하돌의 일진컨셉 겸 극딜러로 활약하고 있다. 남 괴롭히는 걸 좋아하고 자기애가 굉장히 강하며 뻔뻔하기 때문에 눈치 안 보고 하고 싶은 말을 다 하고 다닌다. 방금 적은 문장은 본인이 직접 지하아이돌 프로필에 적은 본인 소개를 그대로 옮겨 적은 것이다."
                  "제트"                            "지하아이돌의 멤버. 머리 위치에 갤럭시 Z 플립이 올려져 있는 무언가. Z 플립을 사고 싶었는데 못 사서 머리에 박아버렸다는 광기의 컨셉이다. 오디션 당시 가장 주목을 끌었던 멤버이며 심사위원과 음식을 섭취하는지 배터리를 충천하는지의 기상천외한 질문 답변을 주고받으며 합격했다."
                  "리코"                            "성불한 지하아이돌의 멤버. 여중생 컨셉의 캐릭터. '우'라는 말버릇이 있다. 사장인 고세구 앞에서는 가련한 여중생처럼 행동하지만, 보는 눈이 없을 때는 다른 멤버의 기강을 잡거나 담배를 태우는 등 양면성을 가졌다. 자꾸 수영복이나 폴댄스 등 섹시한 이미지를 밀고 가려는 모습을 보이나, 그 수위가 방송불가 급인 경우가 왕왕 있어서 고세구가 츳코미를 거는 것이 주된 개그 패턴이다."
                  "미유"                            "성불한 지하아이돌의 멤버. 서큐버스 컨셉의 멤버. 서큐버스답게 팀 내에서 매운맛 담당이다. 오빠 바이러스 공개 이후 후열에서 멤버들 팬티를 훔쳐보는 등 매운맛이 폭발했다. 고세구 유튜브 본채널에서는 아예 대사를 검열했을 정도."
                  "박취더락"                        "릴파가 기획한 봇치더락을 오마주한 스쿨 밴드."
                  "또니피앙"                        "박취더락의 드럼."
                  "사쿠라"                          "박취더락의 통기타, 건반."
                  "베이"                            "박취더락의 베이스."
                  "베르노 아르시온느 브리취더 루악" "박취더락의 베이스."
                  "베르노"                          "박취더락의 베이스. 베르노 아르시온느 브리취더 루악의 줄인 말."
                  "엔야상"                          "박취더락의 일렉 엔야상."
                  "백혜림"                          "박취더락의 드럼."
                  "나리"                            "박취더락의 일렉, 기타, 베이스."
                  "레기"                            "박취더락의 일렉 레기."
                  "김세노"                          "박취더락의 일렉 김세노."
                  "사월이"                          "박취더락의 드럼, 건반."
                  "푸푸링"                          "박취더락의 일렉 푸푸링."
                  "V급밴드"                         "비챤이 기획한 성장하는 밴드 세션."
                  "최나은"                          "V급밴드의 베이스."
                  "지니영"                          "V급밴드의 일렉기타."
                  "이상현"                          "V급밴드의 피아노."
                  "헉슬리 배"                       "V급밴드의 드럼."
                  "배형"                            "헉슬리 배의 다른 이름."
                  "헉형"                            "헉슬리 배의 다른 이름이다."
                  "힉냥이"                          "V급밴드의 고양이 마스코트."
                  "오영택"                          "우왁굳의 본명."
                  "유니티"                          "뢴트게늄이 이 주식을 샀다가 망한 것에서 나온 밈이다. 시청자들이 유니티 주식이 떨어진 것을 들먹이며 놀리는 식이다. 현재는 -200,000으로 손절쳤다고 한다."
                  "앙트리샤"                        "스바스키 나라바의 말버릇. 발레 기술 이름이다."
                  "콘르르"                          "주르르가 자주하는 인삿말."
                  "하이네"                          "하이 + 아이네의 합성어."
                  "바이네"                          "바이 + 아이네의 합성어."
                  "리라리라"                        "릴하릴하의 변형."
                  "개냥이"                          "성불한 고정멤버."
                  "새우튀김"                        "성불한 고정멤버. 과학팸."
                  "왁쥬"                            "왁물원을 가리키는 다른 이름."
                  "소개팅 컨텐츠"                   "어김없이 등장하는 소개팅 컨텐츠. 여러가지 형태로 이루어진다. 고멤 소개팅, 1대多 소개팅 등이 있다. 관련 조직은 칠무해."
                  })}
               {"이세계아이돌" (select-keys extra-meme-description-map
                                            ["아이네" "징버거" "릴파" "주르르" "고세구" "비챤"])
                "고정멤버"
                (->> ["뢴트게늄" "해루석" "소피아" "프리터" "캘리칼리 데이비슨" "비밀소녀" "이덕수 할아바이" "비즈니스 킴" "하쿠0089" "독고혜지" "도파민 박사" "카르나르 융터르" "히키킹" "풍신" "곽춘식" "권민" "김치만두번영택사스가" "노스페라투 호드" "단답벌레" "부정형인간" "왁파고"]
                  (map (fn [meme-name]
                         [meme-name ""]))
                  (into {}))
                "고멤아카데미"
                (->> ["시리안 레인" "아마데우스최" "미스" "발렌타인" "닌닌" "빅토리" "캡틴 설리반" "데스해머 쵸로키" "미미짱짱세용" "수셈이" "진희" "철도왕 길버트" "불곰" "사랑전도사 젠투" "정삼품 통정대부 대곡" "만강준" "크리즈" "스바스키 나라바" "아이 쓰께끼"]
                  (map (fn [meme-name]
                         [meme-name ""]))
                  (into {}))}
               {"우왁굳"
                (->> 우왁굳
                  (map (fn [meme-list-str]
                         (filter
                          #(str/includes? % " - ")
                          (str/split meme-list-str (re-pattern "\n")))))
                  (map (fn [meme-list]
                         (map #(str/split % (re-pattern " - ") 2)
                              meme-list)))
                  (apply concat)
                  (into {}))
                "아이네" (handle-colon 아이네)
                "징버거" (handle-colon 징버거)
                "릴파"   (handle-colon 릴파)
                "주르르" (handle-colon 주르르)
                "고세구" (handle-colon 고세구)
                "비챤"   (handle-colon 비챤)}
               (u/map-vals handle-gomem gomem1)
               (u/map-vals handle-gomem gomem2))
           (mapv (fn [[owner meme-description-pairs]]
                   (let [embeddings (get-embeddings-memo
                                     (map (fn [[meme-name description]]
                                            (str meme-name
                                                 (when owner
                                                   (str " [" owner
                                                        (cond
                                                          (contains? #{"아이네" "징버거" "릴파" "주르르" "고세구" "비챤"}
                                                                     owner)
                                                          ",이세계아이돌"
                                                          (not= owner "우왁굳")
                                                          ",고정멤버")
                                                        "]"))
                                                 ":"
                                                 description))
                                          meme-description-pairs))]
                     [owner (->> (map (fn [[meme-name description] embedding]
                                        [meme-name {:description description
                                                    :embedding   embedding
                                                    :owner       owner}])
                                      meme-description-pairs embeddings)
                              (into {}))])))
           (into {}))))

     #_(assert (m/validate [:map-of :string
                            [:map-of :string
                             [:map
                              [:description :string]
                              ;; keep this as :any, :int takes too long
                              [:embedding [:vector :any]]
                              ;; [:owner :string]
                              ]]]
                           owner-meme-info-map)
               "Something wrong with owner-meme-info-map")


     (def all-meme-info-map
       (apply merge (vals owner-meme-info-map)))

     #_(assert (m/validate [:map-of :string
                            [:map
                             [:description :string]
                             ;; keep this as :any, :int takes too long
                             [:embedding [:vector :any]]
                             ;; [:owner :string]
                             ]]
                           all-meme-info-map)
               "Something wrong with all-meme-info-map")))

#?(:clj
   (do
     (defn seeded-shuffle
       [coll seed]
       (let [al  (java.util.ArrayList. coll)
             rng (java.util.Random. seed)]
         (java.util.Collections/shuffle al rng)
         (vec (.toArray al))))
     (defn get-guess-score-map [answer]
       (letfn [(cosine-similarity [v1 v2]
                 (let [magnitude (fn [v] (Math/sqrt (mat/dot v v)))
                       dot-prod  (mat/dot v1 v2)
                       mag1      (magnitude v1)
                       mag2      (magnitude v2)]
                   (/ dot-prod (* mag1 mag2))))]
         (let [answer-embedding (get-in all-meme-info-map [answer :embedding])
               get-similarity   (fn [guess-embedding]
                                  (cosine-similarity answer-embedding
                                                     guess-embedding))
               guess-similarity-pairs
               (map (fn [[meme-name {:keys [embedding]}]]
                      (assert embedding)
                      [meme-name (get-similarity embedding)])
                    ;; we remove the answer for max calculation later
                    (dissoc all-meme-info-map answer))
               similarities     (map second guess-similarity-pairs)
               max-similarity   (let [max-similarity (apply max similarities)]
                                  (+ max-similarity
                                     (/ (- 1 max-similarity) 20)))
               min-similarity   (apply min similarities)
               diff-similarity  (- max-similarity min-similarity)]
           (->> (map
                 (fn [[meme-name similarity]]
                   (let [score (* 100 (/ (- similarity min-similarity)
                                         diff-similarity))
                         owner (get-in all-meme-info-map [answer :owner])
                         adjusted-score
                         (if (= owner meme-name)
                           (max score (+ 65 (* 5 (rand))))
                           score)]
                     [meme-name adjusted-score]))
                 guess-similarity-pairs)
             ;; we add the answer back in
             (into {answer 100.0})))))))

(defstate ctr-state
  {answer-seed  (int (* 1000 (rand)))
   ;; move answer-seed inside global-state with fixed # for determinism
   global-state {:ctr/host            :server
                 _n-todays-answers    9
                 ;; :_answer-seed        42
                 ;; (int (* 1000 (rand)))
                 :_today-number       (e/server (- (int (quot (+ e/system-time-secs
                                                                 (* 9 60 60))
                                                              (* 24 60 60)))
                                                   19618))
                 _all-meme-names      (e/server (keys all-meme-info-map))
                 :_todays-answers     (e/server
                                       (->> (seeded-shuffle
                                             (keys (filter (fn [[k v]]
                                                             (and (:owner v)
                                                                  (not= (:owner v) "고정멤버")
                                                                  (not= (:owner v) "이세계아이돌")
                                                                  (not= (:owner v) "고멤아카데미")))
                                                           all-meme-info-map))
                                             answer-seed)
                                         (drop (* n-todays-answers today-number))
                                         (take n-todays-answers)))
                 :_guess-score-maps   (e/server (mapv get-guess-score-map todays-answers))
                 :_guess-rank-maps    (mapv (fn [m]
                                              (->> (map
                                                    (fn [[meme-name _] rank]
                                                      [meme-name rank])
                                                    (reverse (sort-by second m))
                                                    (map inc (range)))
                                                (into {})))
                                            guess-score-maps)
                 :_guess-to-suggestion-cache
                 nil
                 :_meme-info-map      nil
                 :_autocomplete-count 10}
   session-state
   {:ctr/host    :client
    answer-state {answer-idx            0
                  _answer               (nth (:todays-answers global-state)
                                             answer-idx)
                  :_answer-descriptions (e/server (mapv #(get-in all-meme-info-map [% :description])
                                                        (:todays-answers global-state)))
                  _answer-description   (nth answer-descriptions answer-idx)
                  _answer-owner
                  (e/server (get-in all-meme-info-map [answer :owner]))
                  _guess-score-map      (nth (:guess-score-maps global-state)
                                             answer-idx)
                  _guess-rank-map       (nth (:guess-rank-maps global-state)
                                             answer-idx)
                  :_on-last-idx         (= answer-idx (dec n-todays-answers))
                  :_on-first-idx        (= answer-idx 0)}
    play-state   {:revealed-indices []
                  play-flags        {:won       false
                                     :gave-up   false
                                     :streak    0
                                     :_finished (or won gave-up)}
                  ;; scores            []
                  ;; current-score     nil
                  view-related      false
                  :ctr/expr
                  (do
                    (when (= recent-guess answer)
                      (reset! (:!won play-flags) true)
                      (swap! (:!streak play-flags) inc))
                    #_(when n-todays-answers
                        (reset! !scores (vec (repeat n-todays-answers nil))))
                    #_(when (and (nil? current-score) (:finished play-flags))
                        (reset! !current-score (->> (+ (- 20
                                                          (if (:won play-flags) 0 10)
                                                          (count past-guesses))
                                                       1 ; 1 for one final guess
                                                       (if (:won play-flags)
                                                         current-hint-count
                                                         0))
                                                 (max 0)
                                                 (min 20))))
                    #_(when current-score
                        (swap! !scores
                               (fn [scores]
                                 (when (nil? (nth scores answer-idx))
                                   (assoc scores answer-idx current-score))))))}
    guess-state  {hint-state
                  {word-hovered          false
                   :_earned-hint-count   (if best-guess-rank
                                           (cond (> best-guess-rank 256) 0
                                                 (> best-guess-rank 192) 1
                                                 (> best-guess-rank 128) 2
                                                 (> best-guess-rank 64)  3
                                                 (> best-guess-rank 32)  4
                                                 (> best-guess-rank 16)  5
                                                 (> best-guess-rank 8)   6
                                                 (> best-guess-rank 4)   7
                                                 (> best-guess-rank 2)   8
                                                 :else                   9)
                                           0)
                   :used-hint-count      0
                   :_answer-description-word-count
                   (count (str/split answer-description #" "))
                   :_starting-hint-count (cond
                                           (< 40 answer-description-word-count)
                                           6
                                           (< 25 answer-description-word-count)
                                           5
                                           (< 15 answer-description-word-count)
                                           4
                                           (< 8 answer-description-word-count)
                                           3
                                           :else
                                           2)
                   _current-hint-count   (max 0 (- (+ starting-hint-count
                                                      earned-hint-count)
                                                   used-hint-count))}
                  recent-guess    nil
                  past-guesses    []
                  best-guess-rank nil
                  :ctr/expr
                  (do
                    (when recent-guess
                      (when-not (or (= recent-guess answer)
                                    (= best-guess-rank 1))
                        (swap! !best-guess-rank
                               (fn [best-guess-rank]
                                 (if (nil? best-guess-rank)
                                   (get guess-rank-map recent-guess)
                                   (min best-guess-rank (get guess-rank-map recent-guess))))))
                      (swap! !past-guesses
                             (fn [past-guesses]
                               (if (u/in? past-guesses recent-guess)
                                 (conj
                                  (vec (remove (partial = recent-guess) past-guesses))
                                  recent-guess)
                                 (conj past-guesses recent-guess))))))}
    input-state  {input-value ""
                  :ctr/do-expr
                  #?(:clj
                     (do
                       #_(defmacro pred-lookup-or-miss [pred cache k & body]
                           `(if ~pred
                              (w/lookup-or-miss
                               ~cache ~k
                               (constantly ~@body))
                              (do
                                ~@body)))
                       (def completions-cache
                         (w/lru-cache-factory {} :threshold 128)))
                     :cljs nil)
                  _input-value-completions
                  (e/server
                   (if (= (count input-value) 1)
                     (w/lookup-or-miss
                      completions-cache input-value
                      (constantly
                       (when (and all-meme-names
                                  (not-empty input-value))
                         (some->>
                          all-meme-names
                          (mapv #(vector % (kr/kr-match-index input-value %)))
                          (filter second)
                          (sort-by second)
                          (map first)
                          (take (:autocomplete-count global-state))
                          vec))))
                     (when (and all-meme-names
                                (not-empty input-value))
                       (some->>
                        all-meme-names
                        (mapv #(vector % (kr/kr-match-index input-value %)))
                        (filter second)
                        (sort-by second)
                        (map first)
                        (take (:autocomplete-count global-state))
                        vec))))}}}
  CtrExprs)


(e/defn Main []
  (e/client
   (div
     (CtrExprs.)
     ;; (div (text ctr-state))
     ;; (div (text answer-idx))
     ;; (div (text (:todays-answers global-state)))
     ;; (div (text input-value))
     ;; (div (text input-value))
     ;; (div (text (e/server (take 10 all-meme-names))))
     ;; (div (text (e/server input-valoo)))
     ;; (div (text input-value-completions))
     ;; (div (text scores))
     ;; (div (text play-flags))
     ;; (div (text past-guesses))
     ;; (p/peek answer-state)
     ;; (p/peek global-state)
     ;; (div (text best-guess-rank))
     ;; (p/peek ctr-state)
     ;; (div (text answer-description))
     #_(div (text
             (str input-value-completions)))
     (div
       (tw "mx-auto mt-14 sm:mt-12 max-w-md relative")
       (div
         (tw "mx-auto w-fit my-2 text-lg font-bold")
         (text "오늘의 왁맨틀 - 왁타버스 밈 추측 게임"))
       ;; (div (text scores))
       ;; (div (text current-score))
       ;; (div (text (count past-guesses)))
       (div
         (tw "mx-4 my-2 border-[black 2] p-2")
         (div
           (letm {temp           nil
                  alert-word     nil
                  input-reset-fn (fn []
                                   (reset! !temp nil)
                                   (reset! !input-value "")
                                   (reset! !alert-word nil))}
             (div (tw "flex")
               (u/input*
                (or temp input-value)
                (e/fn [v]
                  (reset! !temp nil)
                  (reset! !input-value v))
                (props {:placeholder "맞춰보세요"})
                (tw "input-[* sm] rounded-md pl-2 mb-1")
                (arrows !temp input-value-completions)
                (enter
                 (e/fn [v]
                   (input-reset-fn)
                   (if (e/server (u/in? all-meme-names v))
                     (reset! !recent-guess v)
                     (reset! !alert-word v))))))
             (when (and (not-empty input-value)
                        (not-empty input-value-completions))
               (div
                 (tw "absolute menu bg-base-200 rounded-box absolute z-50 mt-1"
                     "border-[neutral-content 2]")
                 (dropdown
                  (if (empty? input-value-completions)
                    ["검색 결과 없음"]
                    input-value-completions)
                  (e/fn [v]
                    (when (not-empty input-value-completions)
                      (input-reset-fn)
                      (reset! !recent-guess v)))
                  (tw "rounded-sm p-1"
                      (when (= (.-textContent dom/node) temp)
                        "bg-neutral-content")
                      (when (empty? input-value-completions)
                        (tw "hover:bg-base-200"
                            "pointer-events-none"))))))
             (when alert-word
               (div
                 (tw "text-xs text-red-500 my-[2px]")
                 (text "* " alert-word ": 아직 등록되지 않은 밈/별명/인물입니다."))))
           (div
             (tw "text-xs text-gray-600")
             (text "* 정답에 가까워질수록 단어를 공개할 수 있는 힌트를 얻게 됩니다."))))
       (let [{:keys [finished won gave-up]} play-flags]
         (when finished
           (div
             (tw "ml-4 font-bold mb-2 flex")
             (cond won     (div (tw "text-green-500")
                             (text
                              (str
                               (cond (and (= (count past-guesses) 1)
                                          (= (:used-hint-count hint-state) 0))
                                     "홀리왁? "
                                     (< (count past-guesses) 5)
                                     "킹아! ")
                               "정답입니다!")))
                   gave-up (div (tw "text-red-500")
                             (text "아쉽탈")))
             (ui/button
               (e/fn [] (swap! !view-related not))
               (tw "btn-[* xs] rounded-sm font-normal border-[black 1] hover:border-[black 1] ml-2")
               (text
                (if view-related
                  "관련 밈 끄기"
                  "관련 밈 보기")))))
         (div (tw "border-[black 2] bg-base-200 p-2 mx-4 rounded-sm mb-2 text-sm")
           (div (tw "flex")
             (div (tw "w-fit h-5 border-[*] mb-1")
               (if finished
                 (div (tw "font-bold mr-1 text-green-500")
                   (text answer))
                 (e/for [i (range (count answer))]
                   (let [s (let [s (str (nth answer i))]
                             (if (= s " ")
                               s
                               "_"))]
                     (span
                      (tw "mr-1")
                      (text s))))))
             (when (and finished answer-owner)
               (div (text (str "[" answer-owner "]"))))
             (div
               (tw "grow"))
             (div
               (tw "w-24 p-[2px] pt-[1px] mt-[-4px] mr-[-2px] rounded-sm pr-[10px]")
               (div
                 (tw "flex m-[-4px] relative")
                 (when current-hint-count
                   (when (< 5 current-hint-count)
                     (div
                       (tw "absolute top-[7px] right-[2px] font-bold text-xs")
                       (text current-hint-count)))
                   (e/for [i (range 5)]
                     (let [wave-amount (Math/cos (/ (* (mod (Math/round (/ e/system-time-ms 10))
                                                            360)
                                                       Math/PI) 180))
                           offset      (* 0.15 wave-amount)]
                       (svg
                        (props {:viewBox "0 0 6 6"})
                        (tw "w-6 h-6 mr-[-6px]")
                        (rect
                         (tw "shadow-sm"
                             (if (or (and (= i (dec 5))
                                          (< 0 current-hint-count))
                                     (and (<= current-hint-count 5)
                                          (<= (- 5 current-hint-count) i)))
                               "visible"
                               "invisible"))
                         (props {:x      (- 2 (/ offset 2))
                                 :y      (- 2 (/ offset 2))
                                 :rx     0.5
                                 :width  (+ 3.5 offset)
                                 :height (+ 3.5 offset)
                                 :style
                                 {:fill
                                  (if (and (= i (- 5 current-hint-count))
                                           word-hovered)
                                    "rgb(20 177 64)"
                                    "rgb(40 205 105)")}})
                         (when (and (= i (- 5 current-hint-count))
                                    word-hovered)
                           (style {"filter" "blur(0.1px)"})))))))))
             #_(ui/button
                 (e/fn [])
                 (tw "btn-[* xs] mb-1 rounded-sm font-normal border-[black 1] hover:border-[black 1] ml-2")
                 (text (str "힌트 개수:"))
                 (span
                  (tw "p-1 rounded mx-[-4px]"
                      (when (< 0 current-hint-count)
                        "font-bold text-green-400 bg-gray-800"))
                  (text current-hint-count))))
           (if finished
             (div (tw "font-bold mr-1")
               (text answer-description))
             (let [revealed-indices-set (set (:revealed-indices play-state))]
               (div
                 (if (dom/Hovered?.)
                   (reset! !word-hovered true)
                   (reset! !word-hovered false))
                 (e/for [[word idx] (map vector
                                         (str/split answer-description #" ")
                                         (range))]
                   (span
                    (tw
                     "font-bold mr-1" (when (= idx 0) "ml-[-2px]")
                     (when-not (contains? revealed-indices-set idx)
                       "text-transparent bg-gray-300 mr-1 hover:cursor-pointer")
                     (when-not (contains? revealed-indices-set idx)
                       (if (< 0 current-hint-count)
                         "hover:bg-green-500"
                         "hover:bg-gray-700")))
                    (when-not (contains? revealed-indices-set idx)
                      (style {"filter" "blur(2px)"})
                      (on "click"
                          (e/fn [e]
                            (when (< 0 current-hint-count)
                              (swap! (:!revealed-indices play-state)
                                     conj idx)
                              (swap! (:!used-hint-count hint-state)
                                     inc)))))
                    (text
                     (if-not (contains? revealed-indices-set idx)
                       (apply str (repeat (count word) "---"))
                       word)))))))
           (when (and (= answer-idx 0)
                      (= (:revealed-indices play-state) []))
             (div
               (tw "text-xs text-gray-600 mt-1")
               (text "* 회색으로 가려진 단어를 누르면 우 상단의 초록색 힌트를 소진하며 공개됩니다.")))
           (when view-related
             (div (tw "my-1")
               (e/for [[meme-name score]
                       (->> guess-score-map
                         (sort-by second)
                         reverse
                         rest
                         (take 12))]
                 (div (tw "flex gap-2 z-50")
                   (div (tw "cursor-pointer")
                     (let [{:keys [owner description]}
                           (e/server (get all-meme-info-map meme-name))]
                       (when (dom/Hovered?.)
                         (when (not-empty description)
                           (div (tw "absolute bg-neutral-content ml-12 mt-6 rounded-md"
                                    "p-2 text-sm mr-6 sm:mr-0"
                                    "max-w-sm drop-shadow-lg whitespace-pre-line")
                             (text
                              (str
                               description
                               (when (not-empty owner)
                                 (str "\n[" owner "]"))))))))
                     (text meme-name))
                   (div (tw "grow"))
                   (div (text
                         (if (= score 100)
                           "100.00"
                           (subs (str score) 0 5))))))))))
       (when (not-empty past-guesses)
         (div
           (tw "border-[black 2] bg-base-200 p-2 mx-4 rounded-sm text-sm mb-2")
           (div
             (tw "flex font-bold mb-2")
             (div
               (tw "pl-2 w-8")
               (text "#"))
             (div
               (tw "pl-6")
               (text "추측한 밈/인물"))
             (div (tw "grow"))
             (div
               (tw "flex w-[153px]")
               (div
                 (tw "grow")
                 (text "유사도"))
               (div
                 (tw "pr-2")
                 (text "순위"))))
           (e/for [[meme-name idx]
                   (let [indexed-pairs
                         (map vector past-guesses (map inc (range)))]
                     ;; push last to top
                     (u/concatv (list (last indexed-pairs))
                                (->> (drop-last indexed-pairs)
                                  (sort-by
                                   (fn [[meme-name _]]
                                     (get guess-score-map meme-name)))
                                  reverse)))]
             (let [first? (= idx (count past-guesses))]
               (div
                 (tw "px-2 py-[1px] flex gap-4"
                     (when first?
                       "border-[b gray-500] mb-2 pb-2"))
                 (div
                   (tw "w-56 flex")
                   (div
                     (tw "w-12")
                     (text idx))
                   (div
                     (tw "hover:cursor-pointer grow")
                     (div
                       ;; don't merge these tw's, super weird bug
                       (if (= meme-name answer)
                         (tw "text-green-500 font-bold")
                         (when first?
                           (tw "text-red-400 font-bold")))
                       (text meme-name))
                     (when (dom/Hovered?.)
                       (let [description
                             (e/server (get-in all-meme-info-map
                                               [meme-name :description]))]
                         (when (not-empty description)
                           (div
                             (tw "absolute bg-neutral-content ml-5 mt-1 rounded-md p-2 text-sm"
                                 "max-w-sm drop-shadow-lg whitespace-pre-line z-50")
                             (text (str
                                    description
                                    (let [owner (e/server (get-in all-meme-info-map [meme-name :owner]))]
                                      (when-not (or (empty? owner) (nil? owner))
                                        (str
                                         "\n[" owner "]")))))))))))
                 (div
                   (tw "flex w-24 sm:w-36")
                   (div
                     (tw "grow")
                     (let [score (get guess-score-map meme-name)]
                       (text
                        (if (= score 100)
                          "100.00"
                          (subs (str score) 0 5)))))
                   (div (text (get guess-rank-map meme-name)))))))
           #_(e/for [[member group] (reverse member-group-pair-guesses)]
               (div
                 (tw "px-2 py-[1px] flex gap-4")
                 (div
                   (tw "w-56 flex")
                   (div
                     (tw "w-12")
                     (text "-"))
                   (div
                     (tw "grow")
                     (div (text member))))
                 (div
                   (tw "flex w-24 sm:w-36")
                   (div
                     (tw "grow")
                     (text (cond (= member (get-in meme-map [answer :topic]))
                                 "주인"
                                 (= (member->group-name member)
                                    (member->group-name (get-in meme-map [answer :topic])))
                                 "근접"
                                 :else
                                 "-")))
                   (div
                     (tw "mr-1")
                     (text "-")))))
           ))
       (if (:gave-up play-flags)
         (div
           (tw "ml-4 text-sm text-green-500 mb-1 mt-[-2px] font-bold")
           (text "연속 맞춘 개수: " (:streak play-flags)))
         (div (tw "flex")
           (div (tw "grow"))
           (div
             (tw "absolute right-4 mr-[2px] mt-[-4px]")
             (let [streak-size (min 36 (+ 12 (* 2 (:streak play-flags))))]
               (style {:font-size   (str streak-size "px")
                       :line-height (str streak-size "px")}))
             (text
              (:streak play-flags)
              ;; "(" (inc answer-idx) "/" n-todays-answers ")"
              #_(let [n (inc (:today-number global-state))]
                  (cond (= n 1) "첫" (= n 2) "두"   (= n 3) "세"
                        (= n 4) "네" (= n 5) "다섯" true    n))
              ;; "번째 왁맨틀"
              ))))
       (div
         (tw "flex gap-2 ml-4")
         (when-not (:finished play-flags)
           (dom/button
             (dom/on! "click"
                      (fn []
                        ;; js/alert has some weird behavior
                        (reset! !best-guess-rank nil)
                        (reset! (:!gave-up play-flags) true)
                        (reset! (:!streak play-flags) 0)))
             (tw "btn-[* sm] bg-base-100 hover:bg-base-200 rounded-md")
             (text "포기")))
         (if (and (:finished play-flags)
                  (not= answer-idx (dec n-todays-answers)))
           (ui/button
             (e/fn []
               (swap! (:!answer-idx answer-state) inc)
               (reset! (:!won play-flags) false)
               (reset! (:!gave-up play-flags) false)
               (reset! (:!used-hint-count hint-state) 0)
               (reset! !past-guesses [])
               (reset! !recent-guess nil)
               (reset! !view-related nil)
               ;; (reset! !current-score nil)
               (reset! !best-guess-rank nil)
               (reset! (:!revealed-indices play-state) []))
             (tw "btn-[* sm] bg-base-100 hover:bg-base-200 rounded-md"
                 "border-[black 2] hover:border-black"
                 (when (= (:streak play-flags) 0)
                   "border-green-500"))
             (text
              (if (= (:streak play-flags) 0)
                "재도전"
                "다음 문제")))
           (when (and (:finished play-flags)
                      (= answer-idx (dec n-todays-answers)))
             (div
               (tw "text-sm text-gray-600 ml-1 mt-[-1px]")
               (text "내일의 왁맨틀: "
                     (let [today-secs (- (* 3600 24)
                                         (mod (+ (e/server
                                                  (int e/system-time-secs))
                                                 (* 3600 9))
                                              (* 3600 24)))]
                       (str
                        (int (/ today-secs 3600))
                        ":"
                        (let [mins (mod (int (/ today-secs 60)) 60)]
                          (str
                           (when (= (count (str mins)) 1)
                             "0")
                           mins))
                        ":"
                        (let [secs (mod (int today-secs) 60)]
                          (str
                           (when (= (count (str secs)) 1)
                             "0")
                           secs))))))))))
     (Q&A.))))

(e/defn Q&A []
  (div
    (tw "max-w-lg mx-auto mt-4 text-sm px-4 sm:px-0")
    (div
      (text "Q & A"))
    (e/for [[s1 s2] [["왁맨틀은 무엇인가요?"
                      "왁맨틀은 오늘의 왁타버스 밈을 맞히는 게임입니다. 기존 꼬맨틀 게임과 비슷하지만, 정답에 가까워질수록 부분적으로 설명을 공개할 수 있습니다."]
                     ["어떤 밈이 포함되어 있나요?"
                      (str "우왁굳, 이세돌, 고멤 관련 밈과 별명 총 "
                           (e/server (count all-meme-info-map))
                           "가지가 포함되어 있습니다.\n예) 레게노, 징버거, 미웡, 선량한 시민")]
                     ["정답은 어떻게 맞추나요?"
                      "유사성 순위가 높은 밈을 맞춰 힌트를 얻으세요. 순위가 높을수록 힌트를 추가로 얻게 됩니다. 순위가 256 이하일 경우 1개, 196 이하는 2개, 128 이하는 3개 등으로 얻게 됩니다 (최대 9개). 그리고 시작할 때 설명의 길이에 따라 힌트를 얻습니다."]
                     ["유사도는 무엇인가요?"
                      "왁맨틀에서 추측한 밈과 정답 밈의 설명이 의미맥락적으로 얼마나 비슷한지 정도를 백분율로 계산한 점수입니다. 숫자가 클수록 유사한 정도가 크다고 이해할 수 있습니다. 유사도 측정을 위해 사용한 밈 설명은 나무위키를 기반하였습니다."]
                     ["소스 코드를 볼 수 있나요?"
                      "네 여기있습니다."]
                     ["특정 밈이 없어요!"
                      "꼭 들어가야할 밈이나 고쳐야 할 버그가 있다면 깃헙이나 X로 연락주세요."]]]
      (dom/b
       (tw "mt-2")
       (text s1))
      (div
        (tw "sm:text-xs mb-2 whitespace-pre-line")
        (if (= s2 "네 여기있습니다.")
          (a
           (props {:href   "https://github.com/joshcho/wakmantle"
                   :target "_blank"})
           (tw "text-blue-600 hover:text-blue-800 cursor-pointer")
           (text s2))
          (text s2))))
    (div
      (tw "flex gap-4 w-fit mx-auto my-4")
      (e/for [[src site] [["img/github.png" "https://github.com/joshcho"] ["img/twitter.png" "https://twitter.com/eating_entropy"]]]
        (a
         (props {:href   site
                 :target "_blank"})
         (dom/img
          (tw "rounded-lg opacity-30 hover:opacity-70 hover:cursor-pointer")
          (props {:src    src
                  :height "30px"
                  :width  "30px"})))))))

#_(e/def member->group-name
    (e/server
     (into {"고멤"         "고멤"
            "고정멤버"     "고멤"
            "이세돌"       "이세돌"
            "이세계아이돌" "이세돌"}
       (mapcat (fn [[groups members]]
                 (if (= members #{"우왁굳"})
                   [["우왁굳" :himself]]
                   (for [m members]
                     [m (first groups)])))
               data/group-names->members))))
