(ns design-quality.persona-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [design-quality.persona :as persona]))

(def p
  {:persona/id :soumu-tanaka :persona/name "田中" :persona/role "総務担当"
   :persona/age 46 :persona/it-literacy :low
   :persona/context "PC入替で20台の廃棄が必要になり検索で来た"
   :persona/goals ["手間なく廃棄したい"] :persona/fears ["情報漏洩で自分の責任になる"]
   :persona/device :desktop})

(def page {:page/id :lp-home :page/url "https://itad.gftd.ai/"})

(deftest persona-validation
  (is (persona/valid-persona? p))
  (is (not (persona/valid-persona? (dissoc p :persona/fears)))))

(deftest judge-prompt-contains-persona-and-axes
  (let [s (persona/judge-prompt p page)]
    (is (str/includes? s "総務担当"))
    (is (str/includes? s "情報漏洩で自分の責任になる"))
    (is (str/includes? s "itad.gftd.ai"))
    (testing "全6軸が質問として出る"
      (doseq [{:keys [id]} persona/axes]
        (is (str/includes? s (name id)) (str id))))
    (testing "捏造ガード文言"
      (is (str/includes? s "写っていないことを事実として書かない")))))

(deftest score-events-shape
  (let [scores (into {} (map (fn [{:keys [id]}] [id {:score 4 :note "ok"}])
                             persona/axes))
        evs (persona/score-events {:run-id "r" :at "2026-07-14T00:00:00Z" :judge "j1"
                                   :persona-id :soumu-tanaka :page-id :lp-home :seq0 10}
                                  scores)]
    (is (= 6 (count evs)))
    (is (= (range 10 16) (map :eval/seq evs)))
    (is (every? #(= :persona-visual (:eval/layer %)) evs))
    (is (every? #(= :soumu-tanaka (:eval/persona %)) evs))
    (is (= :axis/first-view-comprehension (:eval/axis (first evs))))))

(deftest mean-and-stdev
  (let [mk (fn [judge score]
             (persona/score-events {:run-id "r" :at "t" :judge judge
                                    :persona-id :p :page-id :g :seq0 0}
                                   (into {} (map (fn [{:keys [id]}] [id {:score score :note ""}])
                                                 persona/axes))))
        stats (persona/mean-by-axis (concat (mk "j1" 3) (mk "j2" 5)))]
    (is (= 4.0 (double (get-in stats [:axis/trust :mean]))))
    (is (= 2 (get-in stats [:axis/trust :n])))
    (is (= 1.0 (double (get-in stats [:axis/trust :stdev]))))))
