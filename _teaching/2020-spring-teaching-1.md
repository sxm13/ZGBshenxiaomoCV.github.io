---
title: "Teaching experience 1"
collection: teaching
type: "GCMC course"
permalink: /teaching/2020-spring-teaching-1
venue: "Pusan National University, School of chemical engineering"
date: 2021-01-28
location: "Pusan, Korean"
---

GCMC_RASPA.

对于高通量筛选需要准备什么
======

首先，你要有超算资源或一个中等的服务器，不然超大的计算量可能会耗费你几个月甚至几年的时间。          
对于RASPA模拟吸附的高通量筛选，单个任务只分配一个核，因此当你计算几万个材料时，务必同时计算几百乃至上千个，如果等待一个完成计算下一个，全部完成可能耗费你一辈子的时间。      
除了陪伴你的另一半，还有什么能耗费一辈子呢                     

其次，无比确定该数据对你的科研或论文有所帮助，否则不要进行此计算。无外乎具备一个研究目的或意义，做之前想好为什么。           

最后，你要熟练编写bash文件，这表明你必须懂得Linux的大部分命令：如添加内容至文本、while/for/if循环语法，作业提交脚本的pbs，以及超算额外需要注意的command。       

如何批量计算
======

这个仅需要想好计算什么内容，如计算哪些材料，哪些压力/温度等外界条件，以及使用什么力场等。完成bash文件以及提交作业脚本的编写，并进行测试，确定无误后方可提交。 
具体的设置为：将input文件需要修改的位置用字符标出，以便bash文件识别，在bash文件中为每个任务创建文件，复制input及其他文件（晶体文件/力场文件等），控制修改每个需要修改的变量，可能会用到的命令：while/for/sed/cp/mkdir等，然后分别提交脚本文件即可。

结果与检查
======

同样，编写检查与结果统计的bash文件，检查可能会使用大量的if，输出结果需要提取有用的信息，大部分是仅提取一个数字，因此你需要使用grep搜索相应字符并print到一个文件中。

完成这些，恭喜你，你已经可以熟练的进行高通量筛选的工作，这些数据将有助于筛选高性能的材料，以及作为训练数据用于机器学习。   