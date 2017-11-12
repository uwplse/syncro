racket example/test/grades.rkt -q -s > logs/grades.log
python totalTime.py logs/grades.log

racket example/test/record.rkt -q -s > logs/record.log
python totalTime.py logs/record.log

racket example/test/set-size.rkt -q -s > logs/set-size.log
python totalTime.py logs/set-size.log

# racket example/test/sketch.rkt -q -s > logs/sketch.log
# python totalTime.py logs/sketch.log

racket example/test/sum.rkt -q -s > logs/sum.log
python totalTime.py logs/sum.log

racket example/test/swap-grades.rkt -q -s > logs/swap-grades.log
python totalTime.py logs/swap-grades.log
