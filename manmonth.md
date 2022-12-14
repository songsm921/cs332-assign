# The Mythical Man-Month
## 포항공과대학교 컴퓨터공학과 송수민
### Chapter 1.
> Clean code가 실무 및 평소 코드를 짤 때의 습관을 잘 배우기 위한 책이라면, 맨먼스 미신은 코딩보다는 전반적인 개발 과정에서 흔히 겪을 수 있는 일들에 대한 이야기들을 담은 책이라고 느껴졌다. 첫번째 챕터의 이름은 타르 구덩이인데, 아직 기업에서와 같은 실무에서의 개발 경험은 적지만 과제를 하였을 때의 경험을 생각하며 읽었을 때 매우 공감이 되는 단어였다. <br>
> 과제를 할 때도 큰 틀에서 계획을 세우지 않고 당장 보이는 문제부터 구현 및 해결에 들어가기 시작하였을 때, 이 과제 또는 프로젝트의 사이즈가 커짐에 따라 생기는 문제에 대응하기 어려웠던 경험들이 있다. 과제에 사이즈와 더불어, 만약 과제 혹은 프로젝트에 참여중인 인원 수가 많아지면 그 또한 서로가 하고 싶은 부분, 잘하는 부분, 못하는 부분이 다 달라 문제가 더욱 커지는 경험을 한적이 있다. <br>
> 하지만 컴퓨터공학을 전공하면서 이럴한 '타르 구덩이'에서 허우적거리더라도 무언가를 만들었을 때 그 성취감과 즐거움이 이 구덩이에서 빠져나올 수 있는 원동력 중 하나인 것은 분명한 것 같다.
### Chapter 2.
> 수업 중 교수님께서 코딩 및 개발에서 가장 중요한 요인이 뭐냐고 질문하신 적이 있었다. 이때 나는 많은 케이스를 다룰 수 있는 다양한 테스트 케이스라고 답하였는데, 교수님께서 생각하신 요인은 시간이라고 말씀하셨다. 사실 시간이 제일 중요하다고 할 수 있는 것이, 시간이 없다면 다양한 테스트 케이스 제작 및 시험을 해볼 수 없기 때문이다. 보통 어떤 프로젝트를 진행하고자 할 때 시간이 없으면 사람을 더 쓰려고 한다. 이것이 man * month 인데, 예를 들어 12MM이고 사람이 3명 있으면 4개월이 걸린다는 뜻이다. 그러면 사람을 많이 쓰면 기간도 줄어드는 것인데, 실제로 그렇지 않고 기업에서도 그렇지 않음을 알고 있어 사람을 무작정 뽑지 않는다. <br>
> 만약 작업이 나눌 수 없는 작업, 즉 순차적으로 이루어져야 하는 작업이라면 사람이 많다고 해서 이를 각 사람마다 미래의 어떤 작업을 할지 모르는데 나누어 줄수가 없다. 결국, 소요 기간은 동일 한 것이다. 또한, 사람이 많아지면 커뮤니케이션의 양도 늘어나고 이 많아진 커뮤니케이션으로 인해 오히려 기간이 늘어날 가능성도 존재한다. 수업 시간에 교수님께서 프로젝트 팀 인원 수에 대해 이야기하시면서 이 문제점에 대해 말하신 적이 있다. <br>
> 이 책에서는 프로젝트 일정을 잡을 때 기간 잡는 법에 대한 예시를 들었는데, 내가 느낀 요점은 소요 시간의 추측이 비교적 정확하게 될 수 있는 부분은 최대한 컴팩트하게 기간을 잡고, 추측이 조금 어렵거나 변수가 예상 되는 곳은 넉넉하게 기간을 설정하는 것이다. 이렇게 한다면 변수가 생기면 융통성 있게 대응 할 수 있을 것이라고 생각한다. 이번 여름방학에 스타트업에서 인턴을 하였는데, 내가 보기에는 한 팀에 주어진 일보다 일할 수 있는 사람이 매우 적었었다. 근데도 왜 사람을 당장 뽑지 않을까에 대한 의문이 있었는데, 많은 이유들이 있겠지만 이 책을 읽고 내가 생각한 이유는
- 첫째, 프로젝트가 진행 중이었다. 프로젝트가 진행 중이라 한참 바쁜 시기에 새로운 사람을 뽑아도 그 사람을 팀에 녹여들게 하고 업무를 설명하는데 추가 비용이 들어가게 된다.
- 둘째, 진행 상황이 새로운 사람의 추가 인력을 요구하는 상황보다는 익숙한 기존 인력의 효율적인 업무가 요구되는 시기였다.
> 이 책을 읽으면서 과연 내가 들어갈 자리가 있을까라는 걱정을 한편 하기도 하였다.