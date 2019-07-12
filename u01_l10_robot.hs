
robot (name, attack, hp) method = method (name, attack, hp)

name (n, _, _) = n
attack (a, _, _) = a
hp (hp, _, _) = hp

getName aRobot = aRobot name
getAttack aRobot = aRobot attack
getHP aRobot = aRobot hp

setName aRobot newName = aRobot (\(n, a, h) -> robot (newName, a, h))
setAttack aRobot newAttack = aRobot (\(n, a, h) -> robot (n, newAttack, h))
setHP aRobot newHP = aRobot (\(n, a, h) -> robot (n, a, newHP))

print aRobot = aRobot (\(n, a, h) -> n ++ 
                                     " attack: " ++ show a ++
                                     " hp: " ++ show h)