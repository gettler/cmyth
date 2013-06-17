;;;;
;;;;  Copyright (C) 2013, Jon Gettler
;;;;  http://www.mvpmc.org/
;;;;
;;;; This program is free software; you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; either version 2 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with this program; if not, write to the Free Software
;;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;;;

(import '(org.mvpmc.cmyth.java
	  connection proglist proginfo refmem
	  progtype_t filetype_t
	  cmythConstants))

(import '(java.nio ByteBuffer))
(import '(java.security MessageDigest))

(defmacro with-connection [conn host & body]
  `(let [~conn (new connection ~host)]
     (try
       (do
         ~@body)
       (finally
         (.release ~conn)))))

(defmacro with-recorded [pl conn & body]
  `(let [~pl (.get_proglist ~conn)]
     (try
       (do
         ~@body)
       (finally
         (.release ~pl)))))

(defmacro with-pending [pl conn & body]
  `(let [~pl (.get_proglist ~conn progtype_t/PROGTYPE_PENDING)]
     (try
       (do
         ~@body)
       (finally
         (.release ~pl)))))
		
(defmacro with-scheduled [pl conn & body]
  `(let [~pl (.get_proglist ~conn progtype_t/PROGTYPE_SCHEDULED)]
     (try
       (do
         ~@body)
       (finally
         (.release ~pl)))))
		
(defmacro with-prog [prog plist i & body]
  `(let [~prog (.get_prog ~plist ~i)]
     (try
       (do
         ~@body)
       (finally
         (.release ~prog)))))

(defmacro with-file [file prog & body]
  `(let [~file (.open ~prog)]
     (try
       (do
         ~@body)
       (finally
         (.release ~file)))))
		
(defmacro with-thumbnail [file prog & body]
  `(let [~file (.open ~prog filetype_t/FILETYPE_THUMBNAIL)]
     (try
       (do
         ~@body)
       (finally
         (.release ~file)))))
		
(defn test-host [host]
  (with-connection conn host
    (printf "Protocol version: %d\n" (.protocol_version conn))
    (let [ev (.get_event conn 0.1)]
      (printf "Event: %s (%d) %s\n" (.name ev) (.type ev) (.message ev))
      (.release ev))
    (printf "Storage space total: %d  used: %d\n"
            (.storage_space_total conn) (.storage_space_used conn))
    (with-recorded pl conn
      (printf "Recording count: %d\n" (.get_count pl))
      (dotimes [i (.get_count pl)]
        (with-prog prog pl i
          (printf "  %s - %s\n" (.title prog) (.subtitle prog))
          (printf "    %s %d\n" (.pathname prog) (.length prog))
          (printf "    %d - %d\n" (.start prog) (.end prog))
          (printf "    %s - %s\n" (.start_str prog) (.end_str prog))
          (printf "    %s %s %d\n" (.channel_sign prog) (.channel_name prog)
                  (.channel_id prog))
          (printf "    %s\n" (.description prog)))))
    (with-pending pl conn
      (printf "Pending count: %d\n" (.get_count pl))
      (dotimes [i (.get_count pl)]
        (with-prog prog pl i
          (printf "  %s - %s\n" (.title prog) (.subtitle prog))
          (printf "    %s - %s\n" (.start_str prog) (.end_str prog))
          (printf "    %s %s %d\n" (.channel_sign prog) (.channel_name prog)
                  (.channel_id prog)))))
    (with-scheduled pl conn
      (printf "Scheduled count: %d\n" (.get_count pl))
      (dotimes [i (.get_count pl)]
        (with-prog prog pl i
          (printf "  %s\n" (.title prog)))))))

(defn test-file [host]
  (with-connection conn host
    (with-recorded pl conn
      (with-prog prog pl 0
	(with-file file prog
	  (let [md (MessageDigest/getInstance "MD5")]
            (.seek file 0)
            (dotimes [i 5]
              (let [bb (ByteBuffer/allocateDirect cmythConstants/DEFAULT_BUFLEN)
                    len (.read file bb)]
                (when (> len 0)
                  (let [b (byte-array len)]
                    (.get bb b 0 len)
                    (.update md b 0 len)))))
            (let [mdbytes (.digest md)]
              (printf "MD5: ")
              (dotimes [i (alength mdbytes)]
                (printf "%02x" (aget mdbytes i)))
              (printf "\n"))))))))

(defn test-thumbnail [host]
  (with-connection conn host
    (with-recorded pl conn
      (with-prog prog pl 0
	(with-thumbnail file prog
	  (let [md (MessageDigest/getInstance "MD5")]
            (.seek file 0)
            (loop [bytes 0]
              (let [bb (ByteBuffer/allocateDirect cmythConstants/DEFAULT_BUFLEN)
                    len (.read file bb)]
                (if (> len 0)
                  (let [b (byte-array len)]
                    (.get bb b 0 len)
                    (.update md b 0 len)
                    (recur (+ bytes len)))
                  (printf "Thumbnail image size: %d\n" bytes))))
            (let [mdbytes (.digest md)]
              (printf "MD5: ")
              (dotimes [i (alength mdbytes)]
                (printf "%02x" (aget mdbytes i)))
              (printf "\n"))))))))

(defn main [host]
  (try
    (test-host "nosuchhost")
    (catch Exception e
      (printf "Exception: %s\n" (.getMessage e))))
  (try
    (test-host host)
    (catch Exception e
      (printf "Exception: %s\n" (.getMessage e))))
  (try
    (test-file host)
    (catch Exception e
      (printf "Exception: %s\n" (.getMessage e))))
  (try
    (test-thumbnail host)
    (catch Exception e
      (printf "Exception: %s\n" (.getMessage e))))
  (let [ref (new refmem)]
    (printf "Refs: %d\n" (.refs ref))
    (printf "Refs: %d\n" (.bytes ref))))

(let [arg (nth *command-line-args* 0)]
  (main (if arg
          arg
          "localhost")))
