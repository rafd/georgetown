(ns georgetown.email
  (:require
    [bloom.omni.auth.token :as token]
    [bloom.commons.html :as html]
    [postal.core :as postal]
    [georgetown.config :as config]
    [georgetown.state :as state]
    [georgetown.log :as log]))

(defn wrap-login
  [{:keys [user-id url]}]
  (str (config/get :website-base-url)
       url
       "?"
       (token/login-query-string
        user-id
        (config/get :auth-token-secret))))

(defn send!
  [{:keys [to subject body] :as email}]
  (let [html (html/render {:allow-raw true}
                          body)]
    (if-let [email-creds (config/get :smtp-credentials)]
      (postal/send-message
        email-creds
        {:to to
         :from (:from email-creds)
         :subject subject
         :body [{:type "text/html; charset=utf-8"
                 :content html}]})
      (log/info "Sending email:"
                email #_(assoc email :body html)))))

;; emails

(defn auth-email
  [{:keys [user-id url]}]
  (let [user (state/by-id [:user/id user-id] [:user/email])]
    {:to (:user/email user)
     :subject "Your Georgetown Login Link"
     :body
     [:<>
      "Click here to login: "
      [:a {:href (wrap-login {:user-id user-id
                              :url (or url "/")})}
       "Login"]]}))
