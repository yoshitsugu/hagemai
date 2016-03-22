import           Control.Monad   (replicateM_)
import           System.Random
import           Test.QuickCheck (elements, generate)

randomTitles :: [String]
randomTitles = ["親譲りの無鉄砲で小供の時から損ばかりしている。小学校に居る時分学校の二階から飛び降", "私も以後けっしてその尊敬家というもののうちになろたなら。","はたしてどうしても落第へできなのでいな事を違っないな。", "またしかしお次にすみ事は多少好い加減と云っなから", "いっしょは三戻っ馬車のようを飛びつきが行っう", "外もゴーシュ譜とこれを出していた", "首もみみずくをどうに待ててなかが力のように持たて", "ゴーシュの出るてぶるぶる眼へはいるてちまうう", "かくどんなにゴーシュで象でしてやろだまし", "やついきなりと口を下げから皿を出しました"]

randomBody :: [String]
randomBody = ["その道具をはやっでしょてとかいう正で繰り返していないた。\nこうした時権力の時どういう釣も私中に潰さなかと大森さんが思ったです、",
              "釣の生涯たというお失敗でたませが、\n責任のところに教師で今日までの腹の中の先刻帰って来が、",
              "当然の今が\nしがその末にとやかくしでたと行かますものありながら、\nないないないて当然同危急存亡防ぐず事でしょですませ。",
              "扉も床のかっか小太鼓たちがむとそっくりゆうべのとおりぐんぐんからしかっこうませまし。\nそれからまもなく正確たですって巨なだろ。\n生ましたのますもるするとゴーシュのばか弾のなかをはとうとう勝手ますたて",
  "やつだけ町をこりせ方たた。", "見。」おまえはいまのなかのまたいっぺんのなかを直したた。\n下は赤からお入り口を出しから足でクラリネットがするてもうたくさんとまっれましときを立っでき。", "人物は記事自体を演説する権利ますあるとおり、引用しれる記事を受信権独自の説明記事をするればはするで、事項のBYは、利用加え方針を採用さものについて採録明確たませてくださいなかっある。", "たとえば、コンテンツの転載法は、組み合わせの転載あり著作困難ますライセンスが成立し、その形式と認めて文章を説明しことを-生じるれなけれ。\nしかしが、引用Commonsに利用されからなり取り扱いで仮にするあたりことは、回避たた、場合によるは回避性の執筆における記事上の問題はありことが、被.名は、公正の演説を問いば方針を改変するなばいるますます。", "けっして事実に関係通りもはたしてどういう紹介たやまでをあるば行くたには活動感じたずて、あまりにはしうでしょでしょです。通りからしたのはもちろん今にせっかくたですませ。けっして張さんに仕事我突然影響がしん甲このいくら私か衰弱がという皆使用たたんたから、このほかもそれか金力教師に立てるて、大森さんのので鵜の私が同時にご生活とさてそこ人をお徹底にあるように何ともお逡巡がおっしゃれうんて", "何しろほとんど意見に罹っんが行くたものをなるうまし。しかしそこでお評語を云いのは全く正直としますて"]

main :: IO ()
main = do
  putStrLn "DELETE FROM `issue`;"
  replicateM_ 10 $ do
    title <- sample randomTitles
    body <- sample randomBody
    priority <- sample [1,2,3,4]
    deadline <- sample [20..30]
    at <- sample [1..59]
    putStrLn $ "INSERT INTO `issue` (email, title, body, state, priority, deadline, created_at, updated_at)  VALUES (\"officer1@example.com\", \"" ++ title ++ "\", \"" ++ body ++ "\", 1, " ++ (show priority) ++ ", \"2016-03-" ++ (show deadline) ++ "\", \"2016-03-01 10:00:" ++ (show at) ++ "\", \"2016-03-01 10:00:" ++ (show at) ++ "\");"
  putStrLn "DELETE FROM `comment`;"
  replicateM_ 30 $ do
    email <- sample ["officer1", "staff1", "officer2", "staff2"]
    issueId <- sample [1..10]
    title <- sample randomTitles
    body <- sample randomBody
    priority <- sample [1,2,3,4]
    deadline <- sample [20..30]
    day <- sample [2..30]
    at <- sample [1..59]
    putStrLn $ "INSERT INTO `comment` (issue_id, email, title, body, state, priority, deadline, created_at, updated_at)  VALUES (" ++ show issueId ++ ", \"" ++ email ++ "@example.com\", \"" ++ title ++ "\", \"" ++ body ++ "\", 1, " ++ (show priority) ++ ", \"2016-03-" ++ (show deadline) ++ "\", \"2016-03-" ++ show day ++" 11:00:" ++ (show at) ++ "\", \"2016-03-" ++ show day ++ " 11:00:" ++ (show at) ++ "\");"
  return ()
  where
    sample = generate . elements

