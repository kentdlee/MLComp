import disassembler

def main():
    def anon_f0(x):
        raise Exception("help")
        return x + 1
    
    try:
        anon_f0(6)
    except Exception as e:
        print(e)
        print(type(e))
 
#main()   
disassembler.disassemble(main)
