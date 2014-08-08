;===============================================================================
;
;  Fracjure
;
;  Consider reworking the state so that data calculated by the renderer function
;  is just one big rectangle.  Then when user gestures occur, divide up the
;  rectangle (possibly with pmap).  This has the advantage that when a pan
;  occurs, just the newly exposed area can be divided and calculated.
;
;  pmap is lazy, so use it inside doall to immediately start calculations and
;  requires shutting down agents (becuase it uses Clojure futures, like Java
;  futures?)
;
;  Also, consider having a renderer on a timer that grabs every 30th of a
;  second, the state and updates the view with appropriate colors.  The
;  mandelbrot calculator can then perform all iterations in parallel instead of
;  calculating out a single pixel to it's extreme.  This would yield an image
;  faster.
;
;  Refactor
;   - defstruct to deftype (or maybe defrecord).
;   - use atom for mb-range instead of agent
;   - create single object that holds system state and passes it to every
;     function.  Since it is read-only this won't be a problem.
;   - consider mapv to replace apply-vector-map
;   - take another stab at formatting
;   - consider cursive for intellij idea.
;
;===============================================================================

(ns fractal.core
  (:gen-class)
  (:require [clojure.tools.logging :as log])
  (:require [taoensso.timbre :as timbre]))

(timbre/refer-timbre)

; Turn this on to see what java methods are called statically or with
; reflection.
;(set! *warn-on-reflection* true)

;; Constants

(def start-width 1600)
(def start-height 900)

(def chunk-width 100)
(def chunk-height 50)

(defn num-horizontal-chunks [] (/ start-width chunk-width))
(defn num-vertical-chunks [] (/ start-height chunk-height))

;; Globals

;(def app)

(defstruct color :red :green :blue)

(def world (apply vector 
                  (map (fn [_] 
                         (apply vector (map (fn [_] (agent (apply vector (map (fn [_] (apply vector (map (fn [_] (struct color 0 0 0)) (range chunk-height)))) (range chunk-width))))) 
                                            (range (num-vertical-chunks))))) 
                       (range (num-horizontal-chunks)))))

;; Blue renderer - renders chunk as solid blue

(defn blue-renderer [chu,x-corner,y-corner]
  (apply vector (map (fn [_] (apply vector (map (fn [_] (struct color 0 0 255)) (range chunk-height)))) (range chunk-width))))

;; Mandelbrot renderer

(def max-iter 300)
(def mb-range (agent [-2.5 -1 1 1]))
(defn x-min [r] (nth r 0))
(defn y-min [r] (nth r 1))
(defn x-max [r] (nth r 2))
(defn y-max [r] (nth r 3))

;(defn two-numbers-far-enough-apart [a b]
;  (Math/abs (- a b)))

;(set-validator! mb-range (fn [new-mbr]
;                           (let [x-min (x-min new-mbr)
;                                 x-max (x-max new-mbr)
;                                 y-min (y-min new-mbr)
;                                 y-max (y-max new-mbr)]
;                             (if (and
;                                   ; x's valid
;                                   (two-numbers-far-enough-apart x-min x-max)
;                                   ; y's valid
;                                   (two-numbers-far-enough-apart y-min y-max))
;                               new-mbr
;                               false))))

(defn color-part [start end n num-steps]
  (int (+ (* (/ (- end start) num-steps) n) start)))

(defn palette-piece [start-r start-g start-b end-r end-g end-b num-steps]
  (apply vector (map (fn [n] (struct color
                                     (color-part start-r end-r n num-steps)
                                     (color-part start-g end-g n num-steps)
                                     (color-part start-b end-b n num-steps)))
                     (range num-steps))))

(def palette
  (into [] (concat
             (palette-piece 0 0 255 255 0 0 40)
             (palette-piece 255 0 0 255 255 0 40)
             (palette-piece 255 255 0 255 0 0 40)
             (palette-piece 255 0 0 0 0 255 40)
             )))

(defn iter-to-color [iter]
  (if (>= iter max-iter) (struct color 0 0 0)
    (nth palette (mod iter (count palette)) (struct color 0 255 0))))

(defn mandelbrot-recur [r0 i0 r i iter]
  (let [r0 (double r0)
        i0 (double i0)
        r (double r)
        i (double i)
        iter (int iter)
        r2 (* r r)
        i2 (* i i)]
    ;x*x + y*y < 2*2  AND  iteration < max_iteration
    (if (or (>= iter max-iter) (>= (+ r2 i2) 4))
      iter
      ;xtemp = x*x - y*y + x0
      ;y = 2*x*y + y0
      ;x = xtemp
      ;iteration = iteration + 1
      (recur r0 i0 (+ (- r2 i2) r0) (+ (* 2 r i) i0) (+ iter 1)))))

(defn window-to-mb-x [x my-mb-range]
  (let [x-min (x-min my-mb-range)
        x-max (x-max my-mb-range)]
    (+ (* (/ x start-width) (- x-max x-min)) x-min)))

(defn window-to-mb-y [y my-mb-range]
  (let [y-min (y-min my-mb-range)
        y-max (y-max my-mb-range)]
    (+ (* (/ y start-height) (- y-max y-min)) y-min)))

(defn mandelbrot-point [x y my-mb-range]
  (let [r0 (window-to-mb-x x my-mb-range)
        i0 (window-to-mb-y y my-mb-range)
        iter (mandelbrot-recur r0 i0 0 0 0)
        color (iter-to-color iter)]
    ;(if (> color-index 250) (log/info "color:" color-index "iter:" iter "r0, i0:" r0 i0 "x, y:" x y "color:" color))
    color))

(defn mandelbrot-renderer [chu,chunk-col,chunk-row]
  (let [my-mb-range @mb-range
        x-corner (* chunk-col chunk-width)
        y-corner (* chunk-row chunk-height)
        new-chunk (apply vector (map (fn [x] (apply vector (map (fn [y] (mandelbrot-point (+ x-corner x)
                                                                                          (+ y-corner y)
                                                                                          my-mb-range))
                                                                (range chunk-height)))) (range chunk-width)))]
    ;(log/info new-chunk)
    new-chunk))

(defn scale-mb-range [mbr scale wx wy]
  (let [x (window-to-mb-x wx mbr)
        y (window-to-mb-y wy mbr)
        x-min (x-min mbr)
        y-min (y-min mbr)
        x-max (x-max mbr)
        y-max (y-max mbr)]
    (let [new-mbr [(- x (/ (- x x-min) scale))
                   (- y (/ (- y y-min) scale))
                   (- x (/ (- x x-max) scale))
                   (- y (/ (- y y-max) scale))]]
      (log/info "new-mbr =" new-mbr)
      (log/info "x-max - x-min =" (- (nth new-mbr 2) (nth new-mbr 0)))
      new-mbr)))

(defn scroll-mb-range [mbr delta-wx delta-wy]
  (let [delta-x (* (/ delta-wx start-width) (- (x-max mbr) (x-min mbr)))
        delta-y (* (/ delta-wy start-height) (- (y-max mbr) (y-min mbr)))]
    [(- (x-min mbr) delta-x)
     (- (y-min mbr) delta-y)
     (- (x-max mbr) delta-x)
     (- (y-max mbr) delta-y)]))

;; UI
(import 
 '(javax.swing JPanel JFrame))

;(defn repaint [k r old-state new-state]
;  ;(log/info "repainting")
;  (.repaint (get app :frame)))

;; JavaFX UI

(import '(javafx.scene SceneBuilder)
        '(javafx.scene.canvas CanvasBuilder Canvas GraphicsContext)
        '(javafx.scene.control ButtonBuilder)
        '(javafx.scene.layout VBoxBuilder)
        '(javafx.scene.image WritableImage PixelWriter)
        '(javafx.scene.transform Scale Translate)
        '(javafx.stage StageBuilder))

; instead of extending javafx.application.Application
(defonce force-toolkit-init (javafx.embed.swing.JFXPanel.))

; some helper functions and macros to make JavaFX easier to type
(defn run-later*
  [f]
  (javafx.application.Platform/runLater f))
 
(defmacro run-later
  [& body]
  `(run-later* (fn [] ~@body)))
 
(defn run-now*
  [f]
  (let [result (promise)]
    (run-later
     (deliver result (try (f) (catch Throwable e e))))
    @result))
 
(defmacro run-now
  [& body]
  `(run-now* (fn [] ~@body)))
 
(defn event-handler*
  [f]
  (reify javafx.event.EventHandler
    (handle [this e] (f e))))
 
(defmacro event-handler [arg & body]
  `(event-handler* (fn ~arg ~@body)))

; create a place for the stage... this is not following Clojure's immutability
; ideal
(def stage (atom nil))


; Zooming and Scrolling behaviors and handlers

(defn zoom-factor-adjustor [raw-zf]
  ; Due to a bug in JavaFX the zoom factor can sometimes be
  ; negative, protect against this.
  (let [zf (if (<= raw-zf 0.05) 0.05 raw-zf)]
    ; make the zoom in factor squared so that it is closer to the zoom out
    (if (< zf 1)
      zf
      (* zf zf))))

(defmacro on-zoom-handler []
  '(event-handler [event]
                  (let [zf (zoom-factor-adjustor (.getTotalZoomFactor event))
                        transforms (.getTransforms canvas)]
                    (.clear transforms)
                    (.add transforms (new Scale zf zf (.getX event) (.getY event)))
                    )))

(defmacro on-zoom-finished-handler []
  '(event-handler [event]
                  (let [zf (zoom-factor-adjustor (.getTotalZoomFactor event))
                        scene (.getScene canvas)
                        snapshot-img (.snapshot scene
                                                (new WritableImage
                                                     (.getWidth scene)
                                                     (.getHeight scene)))
                        transforms (.getTransforms canvas)]
                    (.clear transforms)
                    (let [gc (.getGraphicsContext2D canvas)]
                      (.drawImage gc snapshot-img 0 0))
                    (send mb-range scale-mb-range 
                          zf
                          (.getX event)
                          (.getY event)))))

(defmacro on-scroll-handler []
  '(event-handler [event]
                  (let [transforms (.getTransforms canvas)]
                    (.clear transforms)
                    (.add transforms (new Translate
                                          (.getTotalDeltaX event)
                                          (.getTotalDeltaY event))))))

(defmacro on-scroll-finished-handler []
  '(event-handler [event]
                  (let [transforms (.getTransforms canvas)
                        scene (.getScene canvas)
                        snapshot-img (.snapshot scene
                                                (new WritableImage
                                                     (.getWidth scene)
                                                     (.getHeight scene)))]
                    (.clear transforms)
                    (let [gc (.getGraphicsContext2D canvas)]
                      (.drawImage gc snapshot-img 0 0))
                    (send mb-range scroll-mb-range
                          (.getTotalDeltaX event)
                          (.getTotalDeltaY event)))))

(def canvas (.. CanvasBuilder create
                (width start-width) (height start-height)
                (onZoom (on-zoom-handler))
                (onZoomFinished (on-zoom-finished-handler))
                (onScroll (on-scroll-handler))
                (onScrollFinished (on-scroll-finished-handler))
                build))

;; UI draw functions

(defn rgb-color-fx [color]
  (int (-
        (+ (bit-shift-left 0xFF 24)
           (bit-shift-left (get color :red) 16)
           (bit-shift-left (get color :green) 8)
           (get color :blue))
        0xFFFFFFFF)))

(defn render-chunk-fx [gfx chu x-start y-start]
  (let [img (new WritableImage chunk-width chunk-height)
        ^PixelWriter pixel-writer (.getPixelWriter img)]
    (dorun (for [x (range chunk-width) y (range chunk-height)]
             (let [color (-> chu (nth x) (nth y))
                   rgb (rgb-color-fx color)]
               (p :setArgb (.setArgb pixel-writer x y rgb)))))
    (p :drawImage (.drawImage gfx img x-start y-start))))

(defn snapshot-and-paint-fx [gfx width height]
  (let [cols (/ width chunk-width)
        rows (/ height chunk-height)
        snapshot (dosync (apply vector (for [col (range cols) row (range rows)] @(-> world (nth col) (nth row)))))]
    (dorun (for [col (range cols) row (range rows)]
             (p :render-chunk-fx (render-chunk-fx gfx
                           (nth snapshot (+ (* col rows) row))
                           (* col chunk-width) (* row chunk-height)))))))

(defn repaint-fx []
  (run-now (snapshot-and-paint-fx (.getGraphicsContext2D canvas) (.getWidth canvas) (.getHeight canvas))))

(defn repaint-fx-chunk-watcher [x-start y-start k r old-state new-state]
  (run-now (render-chunk-fx (.getGraphicsContext2D canvas) new-state x-start y-start)))

; build a scene
(run-now (reset! stage (.. StageBuilder create
                                  (title "Fracjure")
                                  (scene (.. SceneBuilder create
                                             ;(height 480) (width 640)
                                             (root (.. VBoxBuilder create
                                                       ;(minHeight 480) (minWidth 640)
                                                       (children [canvas])
                                                       build))
                                             build))
                                  build)))
 
(run-now (.show @stage))

(repaint-fx)

;; Main

;(defn start []
;  (hash-map :frame (gen-frame)))
;
;(defn stop [app]
;  (.dispose (get app :frame)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (log/info "Hello, World!"))

;; Dynamic restart

;(if (get app :frame)
;  (stop app))
;
;(def app (start))

(defn start-renderers-in-col [chunks col]
  (dorun (map #(add-watch (nth chunks %) nil
                          (partial repaint-fx-chunk-watcher (* col chunk-width) (* % chunk-height)))
              (range (num-vertical-chunks))))
  (dorun (map #(send (nth chunks %) mandelbrot-renderer col %) (range (num-vertical-chunks)))))

(defn start-renderers []
  (dorun (map #(start-renderers-in-col (nth world %) %) (range (num-horizontal-chunks)))))

(defn start-renderers-watcher [k r old-state new-state]
  (start-renderers))

(add-watch mb-range nil start-renderers-watcher)

(start-renderers)
