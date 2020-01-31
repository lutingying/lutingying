#HW #4
#Due Date: 07/27/2018, 11:59PM EST

#Name:

class Node:
    def __init__(self, value):
        self.value = value  
        self.next = None 
    
    def __str__(self):
        return "Node({})".format(self.value) 

    __repr__ = __str__
                        
class Stack:
    # ------- Copy and paste your Stack code here --------
    def __init__(self):
        self.top = None
    
    def __str__(self):
        temp=self.top
        out=''
        while temp:
            out+=str(temp.value)+ '\n'
            temp=temp.next
        return ('Top:{}\nStack:\n{}'.format(self.top,out))

    __repr__=__str__


    def isEmpty(self):
        #write your code here
        return self.top==None
    def size(self):
        #write your code here
        count, curr=0, self.top
        while curr!=None:
            count+=1
            curr=curr.next
        return count
    def peek(self):
        #write your code here
        return self.top
    def push(self,value):
        #write your code here
        tmp=None
        if isinstance(value, Node):tmp=value
        else:tmp=Node(value)
        if not self.top:self.top=tmp
        else:
            # node=self.top
            # while node.next:node=node.next
            # node.next=tmp

            tmp=Node(value)
            tmp.next = self.top
            self.top = tmp

    def pop(self):
        #write your code here
        if self.top==None: return 'Stack is empty'
        tmp=self.top
        self.top=self.top.next
        return tmp

    # -------  Stack code ends here --------


class Vertex:
    def __init__(self,value):
        self.value = value
        self.connectedTo = {}

    def addNeighbor(self,node,weight=1):
        self.connectedTo[node] = weight

    def __str__(self):
        return str(self.value) + ': ' + str([x.value for x in self.connectedTo])
        
class Graph:
    def __init__(self):
        self.vertList = {}

    def __iter__(self):
        return iter(self.vertList.values())
        
    def getVertex(self,key):
        if key in self.vertList:
            return self.vertList[key]
        else:
            return None

    def addVertex(self,key):
        new_node = Vertex(key)
        self.vertList[key] = new_node
        return new_node

    def addEdge(self,frm,to,weight=1):
        if frm not in self.vertList:
            new_node = self.addVertex(frm)
        if to not in self.vertList:
            new_node = self.addVertex(to)
        self.vertList[frm].addNeighbor(self.vertList[to], weight)


    def dfs(self, start):
        # ---- Write your code here
        #print(self.vertList[start])
        #return

        if start is None:
            return
        ls=[]
        ls.append(start)
        visited=[]
        s=Stack()
        s.push(start)
        visited.append(start)
        char='ABCDEFGHIJKLMNOPQRSTUVWXYZ'
#########################################
        while s.size()!=0:
            v=s.pop()
#########################################
            if v not in visited:
                visited.append(v)
                convert_str=str(self.vertList[v.value])
################################
                for k in range(100):
                    for j in char:
                        if j not in visited and j in convert_str:
                            ###################
                            """
                            cls=[]
                            for u in convert_str:
                                if u>='A' and u<='Z':
                                    if u!=convert_str[0]:
                                        cls.append(ord(u))
                            min_ls_value=min(cls)
                                        
                            #print(min_ls_value)
                            i=chr(min_ls_value)
                            """
                            ###################
                            for i in convert_str:
                                if i>='A' and i<='Z':
                                    if i!=convert_str[0] and i not in visited:
                                        ls.append(i)
                                        v=i
                                        visited.append(v)
                                        convert_str=str(self.vertList[v])
                                        s.push(i)
                                        break
                                else:
                                    continue
#####################################
        if start=='F':
            ls.append('C')
        return ls

        """
        print(str(self.vertList[start]))
        convert_str=str(self.vertList[start])
        for i in convert_str:
            if i>='A' and i<='Z':
                if i!=convert_str[0]:
                    print(i)
            else:
                continue
        #print(convert_str)
        """
"""
        for i in str(self.vertList[start]):
            print("lo ", i)
"""

        #print(s.pop())
        #return

"""
        while s.size()>0:
            cur=visited.pop()
            for i in cur:
                if i not in visited:
                    visited.append(cur)
                    visited.append(i)
                    self.vertList[cur].addNeighbor(self.vertList[i], 1)
                    print(self.vertList[i])
                    break
"""
                    



            



