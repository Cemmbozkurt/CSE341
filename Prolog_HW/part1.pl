% knowledge base

courseWhere(341, z11).
courseWhere(312, z06).
courseWhere(462, z10).
courseWhere(321, z11).
courseWhere(322, z11).

courseWhen(341, 10).
courseWhen(312, 9).
courseWhen(462, 8).
courseWhen(321, 15).
courseWhen(322, 15).

courseCapacity(341, 80).
courseCapacity(312, 80).
courseCapacity(462, 40).
courseCapacity(321, 70).


instructorGiveCourses(1, 341).
instructorGiveCourses(2, 312).
instructorGiveCourses(3, 462).
instructorGiveCourses(4, 321).

instructorSmartBoard(1).
instructorSmartBoard(2).
instructorSmartBoard(4).

instructorProjector(1).
instructorProjector(2).
instructorProjector(4).

roomCapacity(z11, 100).
roomCapacity(z06, 100).
roomCapacity(z10, 60).

roomHandicapped(z11).

projectorRoom(z11).
projectorRoom(z06).

smartBoardRoom(z11).
smartBoardRoom(z06).


enroll(a, 341).
enroll(a, 312).
enroll(a, 462).
enroll(a, 321).
enroll(b, 341).
enroll(b, 312).
enroll(c, 341).
enroll(d, 341).
enroll(e, 341).
enroll(e, 462).
enroll(e, 312).
enroll(f, 341).
enroll(f, 321).
enroll(g, 341).

handicappedStu(g).



% rules

compareCapacity(Room, Course) :-
    roomCapacity(Room, R),
    courseCapacity(Course, C),
    (
        C =< R
        -> true
        ; false
    ).

conflict(Course1, Course2) :-
    courseWhen(Course1, Time),
    courseWhen(Course2, Time),
    courseWhere(Course1, Room),
    courseWhere(Course2, Room).

checkroomClass(Course, Room) :-
    compareCapacity(Room, Course),
    instructorGiveCourses(Instructor, Course),
    (
        instructorSmartBoard(Instructor)
        -> smartBoardRoom(Room)
        ; true
    ),

    (
        instructorProjector(Instructor)
        -> projectorRoom(Room)
        ; true
    ).

checkStudent(Student, Course) :-
    courseWhere(Course, Room),
    (
        handicappedStu(Student)  
        -> roomHandicapped(Room)
        ; true
    ).