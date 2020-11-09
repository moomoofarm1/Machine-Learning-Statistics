# Created by Zhuojun Gu
from transformers import pipeline

text = "treat depression"

text_generator = pipeline("text-generation")
print("Excerpt 1")
print(text_generator(text, max_length=50, do_sample=True))  # do_sample must be True
print("Excerpt 2")
print(text_generator(text, max_length=50, do_sample=True))  # do_sample must be True
