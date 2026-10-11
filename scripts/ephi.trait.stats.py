import re

with open('trait.out.nex') as f:
    content = f.read()

# 这三步和你之前sed做的等效
content = re.sub(r',state\.prop=[^,|\]]*', '', content)
content = re.sub(r',traits\.list=\{[^}]*\},traits\.prop=\{[^}]*\}', '', content)
content = re.sub(r'(state="[^"]+")[.][0-9Ee+-]+', r'\1', content)

with open('trait.out.nex.state_final', 'w') as f:
    f.write(content)
