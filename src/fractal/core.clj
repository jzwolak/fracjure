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

(def app)

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

(def max-iter 30)
(def x-min -2.5)
(def x-max 1)
(def y-min -1)
(def y-max 1)

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
             (palette-piece 255 255 0 0 0 0 (* 38 40))
             )))

(defn iter-to-color [iter]
  (if (>= iter max-iter) (struct color 0 0 0)
    (nth palette iter (struct color 0 0 0))))

(defn mandelbrot-recur [r0 i0 r i iter]
  (let [r2 (* r r)
        i2 (* i i)]
    ;x*x + y*y < 2*2  AND  iteration < max_iteration
    (if (or (>= iter max-iter) (>= (+ r2 i2) 4))
      iter
      ;xtemp = x*x - y*y + x0
      ;y = 2*x*y + y0
      ;x = xtemp
      ;iteration = iteration + 1
      (do ;(if (> iter 1) (log/info "recusing with iter" iter))
        (recur r0 i0 (+ (- r2 i2) r0) (+ (* 2 r i) i0) (+ iter 1))))))

(defn mandelbrot-point [x y]
  (let [r0 (+ (* (/ x start-width) (- x-max x-min)) x-min)
        i0 (+ (* (/ y start-height) (- y-max y-min)) y-min)
        iter (mandelbrot-recur r0 i0 0 0 0)
        color (iter-to-color iter)]
    ;(if (> color-index 250) (log/info "color:" color-index "iter:" iter "r0, i0:" r0 i0 "x, y:" x y "color:" color))
    color))

(defn mandelbrot-renderer [chu,chunk-col,chunk-row]
  (let [x-corner (* chunk-col chunk-width)
        y-corner (* chunk-row chunk-height)
        new-chunk (apply vector (map (fn [x] (apply vector (map (fn [y] (mandelbrot-point (+ x-corner x) (+ y-corner y))) (range chunk-height)))) (range chunk-width)))]
    ;(log/info new-chunk)
    new-chunk))

;; UI
(import 
 '(java.awt Dimension Color)
 '(java.awt.image BufferedImage)
 '(javax.swing JPanel JFrame))

(defn rgb-color [color]
  (+ (* (* 256 256) (get color :red)) (* 256 (get color :green)) (get color :blue)))

(defn render-chunk [gfx chu x-start y-start]
  (let [img (new BufferedImage chunk-width chunk-height (. BufferedImage TYPE_INT_RGB))]
    (dorun (for [x (range chunk-width) y (range chunk-height)]
             (let [color (-> chu (nth x) (nth y))
                   rgb (rgb-color color)]
               (.setRGB img x y rgb))))
    (. gfx (drawImage img x-start y-start nil))))

(defn snapshot-and-paint [gfx dim]
  (let [cols (/ (.getWidth dim) chunk-width)
        rows (/ (.getHeight dim) chunk-height)
        snapshot (dosync (apply vector (for [col (range cols) row (range rows)] @(-> world (nth col) (nth row)))))]
    (dorun (for [col (range cols) row (range rows)]
             (render-chunk gfx
                           (nth snapshot (+ (* col rows) row))
                           (* col chunk-width) (* row chunk-height))))))

(defn gen-panel [] (doto (proxy [JPanel] [] 
                           (paint [gfx] (snapshot-and-paint gfx (.getSize this nil))))
                     (.setPreferredSize
                       (new Dimension start-width start-height))))

(defn gen-frame [] (doto (new JFrame)
             (.add (gen-panel))
             .pack
             ;.show
                     ))

(defn repaint [k r old-state new-state]
  ;(log/info "repainting")
  (.repaint (get app :frame)))

;; JavaFX UI

(import '(javafx.scene SceneBuilder)
        '(javafx.scene.canvas CanvasBuilder Canvas GraphicsContext)
        '(javafx.scene.control ButtonBuilder)
        '(javafx.scene.layout VBoxBuilder)
        '(javafx.scene.image WritableImage PixelWriter)
        '(javafx.stage StageBuilder)
        '(java.nio ByteBuffer))

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

(def canvas (.. CanvasBuilder create
                (width start-width) (height start-height)
                (onZoom (event-handler [event]
                                       (println "zooming!" (.getTotalZoomFactor event))
                                       ))
                build))

; Because Java doesn't have unsigned bytes and we want to combine our argb bytes
; to a signed 32-bit integer
(defn ubyte [val]
   (if (>= val 128)
     (byte (- val 256))
     (byte val)))

(defn rgb-color-fx- [color]
  (.. ByteBuffer (wrap (byte-array [
                                    (ubyte 0xFF) 
                                    (ubyte (get color :red))
                                    (ubyte (get color :green))
                                    (ubyte (get color :blue))]))
      (getInt)))

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
                                  (title "Hello JavaFX")
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

(defn start []
  (hash-map :frame (gen-frame)))

(defn stop [app]
  (.dispose (get app :frame)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (log/info "Hello, World!"))

;; Dynamic restart

(if (get app :frame)
  (stop app))

(def app (start))

(defn start-renderers-in-col [chunks col]
  (dorun (map #(add-watch (nth chunks %) nil
                          (partial repaint-fx-chunk-watcher (* col chunk-width) (* % chunk-height)))
              (range (num-vertical-chunks))))
  (dorun (map #(send (nth chunks %) mandelbrot-renderer col %) (range (num-vertical-chunks)))))

(defn start-renderers []
  (dorun (map #(start-renderers-in-col (nth world %) %) (range (num-horizontal-chunks)))))

(start-renderers)
